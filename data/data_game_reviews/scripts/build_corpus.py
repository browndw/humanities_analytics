"""Build a downsampled, English-only, genre-labeled corpus from data/game_reviews.

Selects a handful of popular, lexically-distinct genres, pulls reviews from a
shuffled sample of games in each genre, filters to English by measuring the
per-word-span English ratio with lingua (robust to reviews that splice
together text in multiple languages), and stops once each genre hits a target
word budget, optionally balanced 50/50 between Recommended and Not Recommended
reviews. Output is small enough to ship into a Colab notebook for a
collocations-vs-embeddings activity.

Usage:
    conda run -n ds_env python scripts/build_corpus.py
"""
import argparse
import csv
import json
import random
import sys
from pathlib import Path

from lingua import Language, LanguageDetectorBuilder
from tqdm import tqdm

ROOT = Path(__file__).resolve().parent.parent
GAMES_JSON = ROOT / "data" / "games.json"
REVIEWS_DIR = ROOT / "data" / "game_reviews"

# Priority order used to assign a single genre label to games whose genres
# list matches more than one target genre. Rarer/more specific genres are
# checked before the generic "Action" bucket so e.g. an "Action RPG" is
# labeled RPG, keeping the Action bucket closer to pure shooters/brawlers.
GENRE_PRIORITY = ["RPG", "Strategy", "Simulation", "Action"]

DETECT_LANGUAGES = [
    Language.ENGLISH, Language.GERMAN, Language.FRENCH, Language.SPANISH,
    Language.PORTUGUESE, Language.RUSSIAN, Language.KOREAN, Language.CHINESE,
    Language.JAPANESE, Language.POLISH, Language.TURKISH, Language.ITALIAN,
    Language.DUTCH, Language.SWEDISH, Language.THAI, Language.VIETNAMESE,
    Language.ARABIC,
]


def assign_genre(genres, target_genres):
    """Return the single target genre a game should be bucketed under, or None."""
    game_genres = set(genres or [])
    for genre in GENRE_PRIORITY:
        if genre in target_genres and genre in game_genres:
            return genre
    return None


def load_game_index(target_genres):
    """Map each target genre to a shuffled list of (appid, name, review_csv_path)."""
    with open(GAMES_JSON, encoding="utf-8") as f:
        games = json.load(f)

    buckets = {genre: [] for genre in target_genres}
    for csv_path in REVIEWS_DIR.glob("*.csv"):
        appid = csv_path.stem.split("_")[0]
        game = games.get(appid)
        if not game:
            continue
        genre = assign_genre(game.get("genres"), target_genres)
        if genre is None:
            continue
        buckets[genre].append((appid, game.get("name", ""), csv_path))
    return buckets


def clean_text(text):
    return " ".join(text.split())


def english_word_ratio(text, detector):
    """Fraction of words lingua attributes to English, scanning mixed-language
    spans rather than voting on the whole document. Catches reviews that splice
    together an English blurb with a non-English one (e.g. "ENG: ... / FR: ..."),
    which a single whole-text detect_language_of call can misclassify as English.
    """
    total_words = 0
    english_words = 0
    for segment in detector.detect_multiple_languages_of(text):
        span = text[segment.start_index:segment.end_index]
        n = len(span.split())
        total_words += n
        if segment.language == Language.ENGLISH:
            english_words += n
    return (english_words / total_words) if total_words else 0.0


def build_corpus(args):
    random.seed(args.seed)
    target_genres = args.genres
    buckets = load_game_index(target_genres)

    for genre in target_genres:
        random.shuffle(buckets[genre])
        print(f"{genre:12s} candidate games: {len(buckets[genre])}", file=sys.stderr)

    detector = LanguageDetectorBuilder.from_languages(*DETECT_LANGUAGES).build()

    rows = []
    stats = {
        genre: {"words": 0, "docs": 0, "games": 0, "words_by_recommend": {}}
        for genre in target_genres
    }
    doc_id = 0

    for genre in target_genres:
        target_words = args.words_per_genre
        # When balancing, each recommend/not-recommended bucket gets half the budget.
        target_per_bucket = target_words / 2 if args.balance_recommend else None
        pbar = tqdm(total=target_words, desc=genre, unit="words")
        for appid, name, csv_path in buckets[genre]:
            if stats[genre]["words"] >= target_words:
                break
            game_words = 0
            try:
                with open(csv_path, newline="", encoding="utf-8", errors="replace") as f:
                    reader = list(csv.DictReader(f))
            except Exception:
                continue
            random.shuffle(reader)

            game_used = False
            for row in reader:
                if stats[genre]["words"] >= target_words:
                    break
                if game_words >= args.max_words_per_game:
                    break

                text = clean_text(row.get("review") or "")
                words = text.split()
                if not (args.min_review_words <= len(words)):
                    continue
                if len(words) > args.max_review_words:
                    words = words[: args.max_review_words]
                    text = " ".join(words)

                recommend = row.get("recommend", "") or "Unknown"
                if target_per_bucket is not None:
                    bucket_words = stats[genre]["words_by_recommend"].get(recommend, 0)
                    if bucket_words >= target_per_bucket:
                        continue

                if english_word_ratio(text, detector) < args.min_english_ratio:
                    continue

                doc_id += 1
                rows.append({
                    "doc_id": doc_id,
                    "appid": appid,
                    "game_name": name,
                    "genre": genre,
                    "review": text,
                    "word_count": len(words),
                    "recommend": row.get("recommend", ""),
                    "playtime": row.get("playtime", ""),
                    "post_date": row.get("post_date", ""),
                })
                stats[genre]["words"] += len(words)
                stats[genre]["docs"] += 1
                stats[genre]["words_by_recommend"][recommend] = (
                    stats[genre]["words_by_recommend"].get(recommend, 0) + len(words)
                )
                game_words += len(words)
                game_used = True
                pbar.update(len(words))

            if game_used:
                stats[genre]["games"] += 1
        pbar.close()

        if target_per_bucket is not None and stats[genre]["words"] < target_words:
            print(
                f"WARNING: {genre} ran out of candidate games before reaching the "
                f"balanced target ({stats[genre]['words']}/{target_words} words). "
                f"By bucket: {stats[genre]['words_by_recommend']}",
                file=sys.stderr,
            )

    return rows, stats


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--genres", nargs="+", default=["Action", "RPG", "Strategy", "Simulation"])
    parser.add_argument("--words-per-genre", type=int, default=400_000)
    parser.add_argument("--max-words-per-game", type=int, default=1500,
                         help="Cap per game so the corpus draws from many games, not just a few big ones.")
    parser.add_argument("--min-review-words", type=int, default=8)
    parser.add_argument("--max-review-words", type=int, default=300)
    parser.add_argument("--min-english-ratio", type=float, default=0.9,
                         help="Minimum fraction of a review's words lingua must attribute to English.")
    parser.add_argument("--balance-recommend", action=argparse.BooleanOptionalAction, default=True,
                         help="Split each genre's word budget evenly between Recommended and Not Recommended reviews.")
    parser.add_argument("--seed", type=int, default=13)
    parser.add_argument("--output", default=str(ROOT / "data" / "corpus" / "game_reviews_corpus.csv"))
    args = parser.parse_args()

    rows, stats = build_corpus(args)

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=[
            "doc_id", "appid", "game_name", "genre", "review",
            "word_count", "recommend", "playtime", "post_date",
        ])
        writer.writeheader()
        writer.writerows(rows)

    stats_path = out_path.with_name(out_path.stem + "_stats.json")
    with open(stats_path, "w", encoding="utf-8") as f:
        json.dump(stats, f, indent=2)

    total_words = sum(s["words"] for s in stats.values())
    print(f"\nWrote {len(rows)} reviews ({total_words} words) to {out_path}")
    print(f"Stats written to {stats_path}")
    for genre, s in stats.items():
        by_rec = ", ".join(f"{k}={v}" for k, v in s["words_by_recommend"].items())
        print(f"  {genre:12s} words={s['words']:8d}  docs={s['docs']:6d}  games={s['games']:5d}  ({by_rec})")


if __name__ == "__main__":
    main()
