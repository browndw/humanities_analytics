"""Tag data/corpus/game_reviews_corpus.csv with DocuScope + CLAWS7 and save the
resulting tokens table for the collocation half of the workshop activity
(docuscospacy's coll_table gives raw window co-occurrence counts alongside
PMI-family association measures, computed on the exact same corpus used to
train our word2vec model).

Usage:
    conda run -n ds_env python scripts/tag_corpus.py
"""
import argparse
import csv
import time
from pathlib import Path

import polars as pl
import spacy
import docuscospacy as ds

ROOT = Path(__file__).resolve().parent.parent
CORPUS_CSV = ROOT / "data" / "corpus" / "game_reviews_corpus.csv"

SANITY_CHECK_WORDS = [
    "bug", "boss", "grind", "graphics", "controls", "refund", "story", "combat",
]


def load_corpus():
    with open(CORPUS_CSV, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))
    return pl.DataFrame({
        "doc_id": [r["doc_id"] for r in rows],
        "text": [r["review"] for r in rows],
    })


def sanity_check(tokens, preceding, following, statistic, min_freq_span):
    print("\nTop tokens by raw frequency (dominated by function words, as expected):")
    freq = ds.frequency_table(tokens, count_by="pos")
    print(freq.sort("AF", descending=True).head(10))

    for word in SANITY_CHECK_WORDS:
        coll = ds.coll_table(
            tokens, word, preceding=preceding, following=following,
            statistic=statistic, count_by="pos",
        )
        if coll.height == 0:
            print(f"\n{word!r}: not found in corpus")
            continue

        raw_top = coll.sort("Freq Span", descending=True).head(6)
        pmi_top = (
            coll.filter(pl.col("Freq Span") >= min_freq_span)
            .sort("MI", descending=True)
            .head(6)
        )
        print(f"\n{word!r} -- raw co-occurrence (window ±{preceding}/{following}):")
        print(raw_top.select(["Token", "Freq Span"]))
        print(f"{word!r} -- top {statistic} (Freq Span >= {min_freq_span}):")
        print(pmi_top.select(["Token", "Freq Span", "MI"]))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--preceding", type=int, default=5)
    parser.add_argument("--following", type=int, default=5)
    parser.add_argument("--statistic", default="npmi", choices=["pmi", "npmi", "pmi2", "pmi3"])
    parser.add_argument("--min-freq-span", type=int, default=5,
                         help="Minimum window co-occurrence count before trusting a PMI ranking.")
    parser.add_argument("--batch-size", type=int, default=50)
    parser.add_argument("--n-process", type=int, default=1)
    parser.add_argument("--output", default=str(ROOT / "data" / "tagged" / "game_reviews_tagged.parquet"))
    args = parser.parse_args()

    print("Loading corpus...")
    corp = load_corpus()
    print(f"  {corp.height} documents")

    print("Loading en_docusco_spacy...")
    nlp = spacy.load("en_docusco_spacy")

    print("Tagging corpus (DocuScope + CLAWS7)...")
    t0 = time.time()
    tokens = ds.docuscope_parse(corp, nlp, n_process=args.n_process, batch_size=args.batch_size)
    print(f"  {tokens.height} tokens in {time.time() - t0:.1f}s")

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    tokens.write_parquet(out_path)
    print(f"Saved tagged tokens to {out_path} ({out_path.stat().st_size / 1e6:.1f} MB)")

    sanity_check(tokens, args.preceding, args.following, args.statistic, args.min_freq_span)


if __name__ == "__main__":
    main()
