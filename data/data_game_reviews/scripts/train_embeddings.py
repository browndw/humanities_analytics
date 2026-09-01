"""Train a small word2vec model on data/corpus/game_reviews_corpus.csv.

This gives the workshop a set of embeddings trained on the *exact* same data
used for the collocation/PMI analysis (docuscospacy), so students can compare
count-based association measures and predictive embeddings on one corpus
instead of comparing our small corpus to a vector space trained elsewhere.

The corpus is small (~1.6M words) by word2vec standards, so this is a toy,
illustrative model, not a production embedding space -- treat neighbor lists
as "good enough to see the pattern," not as ground truth.

Usage:
    conda run -n ds_env python scripts/train_embeddings.py
"""
import argparse
import re
import time
from pathlib import Path

import spacy
from gensim.models import Word2Vec

ROOT = Path(__file__).resolve().parent.parent
CORPUS_CSV = ROOT / "data" / "corpus" / "game_reviews_corpus.csv"

# Keep plain words and simple contractions ("don't" -> "do" + "n't"); drop
# numbers, punctuation-only tokens, and symbols.
TOKEN_PATTERN = re.compile(r"^[a-z]+(?:'[a-z]+)?$")

SANITY_CHECK_WORDS = [
    "bug", "boss", "grind", "graphics", "controls", "refund", "story", "combat",
]


def load_reviews():
    import csv
    with open(CORPUS_CSV, newline="", encoding="utf-8") as f:
        return [row["review"] for row in csv.DictReader(f)]


def tokenize_sentences(reviews, min_sentence_tokens=3):
    """Sentence-split and tokenize reviews with a fast rule-based spaCy pipeline."""
    nlp = spacy.blank("en")
    nlp.add_pipe("sentencizer")

    sentences = []
    for doc in nlp.pipe(reviews, batch_size=200):
        for sent in doc.sents:
            tokens = [t.text.lower() for t in sent if TOKEN_PATTERN.match(t.text.lower())]
            if len(tokens) >= min_sentence_tokens:
                sentences.append(tokens)
    return sentences


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--vector-size", type=int, default=100)
    parser.add_argument("--window", type=int, default=5)
    parser.add_argument("--min-count", type=int, default=5)
    parser.add_argument("--epochs", type=int, default=15)
    parser.add_argument("--sg", type=int, default=1, choices=[0, 1],
                         help="1 = skip-gram (better for smaller corpora/rare words), 0 = CBOW")
    parser.add_argument("--negative", type=int, default=10)
    parser.add_argument("--sample", type=float, default=1e-3)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--seed", type=int, default=13)
    parser.add_argument("--output", default=str(ROOT / "data" / "embeddings" / "game_reviews_w2v.bin"))
    args = parser.parse_args()

    print("Loading corpus...")
    reviews = load_reviews()
    print(f"  {len(reviews)} reviews")

    print("Tokenizing sentences...")
    t0 = time.time()
    sentences = tokenize_sentences(reviews)
    n_tokens = sum(len(s) for s in sentences)
    print(f"  {len(sentences)} sentences, {n_tokens} tokens ({time.time() - t0:.1f}s)")

    print("Training word2vec...")
    t0 = time.time()
    model = Word2Vec(
        sentences=sentences,
        vector_size=args.vector_size,
        window=args.window,
        min_count=args.min_count,
        sg=args.sg,
        negative=args.negative,
        sample=args.sample,
        workers=args.workers,
        epochs=args.epochs,
        seed=args.seed,
    )
    print(f"  trained in {time.time() - t0:.1f}s, vocab size={len(model.wv)}")

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    model.wv.save_word2vec_format(str(out_path), binary=True)
    print(f"Saved vectors to {out_path} ({out_path.stat().st_size / 1e6:.1f} MB)")

    print("\nSanity check -- nearest neighbors:")
    for word in SANITY_CHECK_WORDS:
        if word not in model.wv:
            print(f"  {word!r}: not in vocab")
            continue
        neighbors = model.wv.most_similar(word, topn=8)
        neighbor_str = ", ".join(f"{w} ({s:.2f})" for w, s in neighbors)
        print(f"  {word:10s} -> {neighbor_str}")


if __name__ == "__main__":
    main()
