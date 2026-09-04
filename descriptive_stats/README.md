# Descriptive Stats (next session)

Placeholder for the exploratory data analysis (EDA) on the GNH 2013-2017 scores. Not
started yet — scope still needs to be agreed together before any figures get produced.

## What's available going in

* `gnh_panel` (built inside `Data Preparation_GNH 2013-7.R`, not currently saved to
  disk): one row per respondent per year, 2013-2017, with every harmonised item column,
  every sub-domain score, every domain score, and the overall `gnh` score, all on a 0-1
  scale, plus the survey `weight`.
* `agg_data/gnh_turkey_2013_2017.csv`: the one weighted-mean-per-year summary already
  produced from that panel — a starting point, not a substitute for the EDA itself.
* `questionnaire_mapping/swbi_gnh_domain_mapping.csv` and the README's GNH section for
  what each domain/sub-domain means and how its score is built.

## Still to decide before starting

* What the EDA is actually for — a report, a set of charts, an input into a later
  model — since that changes how much rigour and reconciliation it needs.
* Which slice: all nine domains, or a subset; the full 2013-2017 panel, or one year;
  national only, or broken down by a demographic/regional variable.
* Whether `gnh_panel` needs to be saved to disk first (right now it only exists inside
  the R session that runs `Data Preparation_GNH 2013-7.R`), or rebuilt at the start of
  each analysis.

Once scope is settled, this looks like the kind of multi-day exploratory work the
`traceable-figures` skill is meant for (frozen cutoff date, a declared missingness
rule, value tiers, reconciliation, a provenance ledger) rather than a same-day
quick-answer — worth loading that skill at the start of the session rather than
improvising the rigour ad hoc.
