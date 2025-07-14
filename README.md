# From Canon to Score: Quantifying, Measuring, and Comparing Canonisation (DH2025)

This repo accompanies my project on measuring canonisation in literary history using scalable, transparent methods.

## Overview

I work with two prose corpora—English and German fiction from 1688 to 1914 -- compiled from a broad range of literary histories. Each text is assigned a **canonisation score** between 0 and 1, based on:

- how often it appears in literary histories and companions,
- whether student editions exist,
- and whether a collected works edition exists.

I use **logistic regression** to combine these indicators, anchored by:

- texts found on university reading lists (high canonisation),
- and texts barely mentioned at all (low canonisation).

## Embeddings & Temporal Analysis

Once the scores are in place, I use **doc2vec embeddings** to analyze how texts relate to earlier and later works—essentially mapping **influence and legacy** across time. This helps surface patterns in the canon and beyond, and allows comparisons between English and German literary traditions.

## Contents

This repo includes:

- Code and data for calculating canonisation scores
- Rolling centroid analyses based on document embeddings

## Full Project

Corpora, additional data, and full codebase available here:  
👉 [RelatingTheUnread](https://github.com/jbrottrager/RelatingTheUnread)
