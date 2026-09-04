# Türkiye Mutluluk Atlası

This repository holds follow-up work building on the article and book below, sharing
summary findings from further analysis of TÜİK's Life Satisfaction Survey (LSS).

* [The Determinants of Happiness in Turkey: Evidence from City-Level Data](https://link.springer.com/article/10.1007%2Fs10902-016-9746-9)
* [Subjective Well-Being in an Era of Relentless Growth: The Case of Turkey Between 2004 and 2014](https://www.springerprofessional.de/en/subjective-well-being-in-an-era-of-relentless-growth-the-case-of/12232960)
* [Türkiye Mutluluk Atlası](http://www.ituyayinlari.com.tr/kitapdetay.asp?KitapID=539&turkiye-mutluluk-atlasi)

The colleagues who made those works possible, and who continue to give their input on
this project, are listed below.

* [Doç. Dr. Ahmet Atıl Aşıcı](https://www.researchgate.net/profile/Ahmet_Asici)
* [Uğurcan Acar](https://www.researchgate.net/profile/Ugurcan_Acar)

## Data

The underlying microdata is shared by TÜİK on the condition that it is not
redistributed. The raw yearly files live under `data/` and are not tracked in this
repository; only aggregated, weighted output (under `agg_data/`) is shared. `anketler/`
holds the original TÜİK questionnaire documentation (xlsx) used to resolve which raw
column each variable lives under in a given year, and what its answer options mean.

## What's in this repository

### SWBI: the original 6-domain GNH index (2004-2017)

`Data Preparation_SWBI.R`

A Bhutan-style Gross National Happiness index ported from an earlier Stata pipeline,
covering 2004-2017 (2003 is excluded as its questionnaire schema does not line up with
later years, and 2014 is excluded from the final panel because the original Stata
append step never covered it either). It groups roughly 40 survey variables into six
domains: job and income, personal outlook (happiness, hope, and self-assessed future),
health satisfaction, an education/municipal-services/health-problems composite, local
governance and personal safety, and community relationships. Two versions of the index
are produced: a simple-average Alkire-Foster construction (a domain counts as
"sufficient" once its score passes a 2/3 threshold; someone is "happy" once enough
domains clear that bar) and a factor-weighted alternative using weights carried over
from the original Stata factor analysis rather than recomputed here. Output is a
survey-weighted yearly summary, written to `agg_data/swbi_turkey_2003_2017.csv`.

### Logit: happiness logistic regression (2013-2017)

`Data Preparation_Logit.R`, `questionnaire_mapping/ologit_questionnaire_mapping.csv`

Despite the informal name "ologit" used for this piece of work, the regression itself
is an ordinary binary logistic regression (`glm(..., family = "binomial")`), not a
proportional-odds ordinal logit — the script's own comment says so directly ("Only
works for 0/1 y variables. Not Ologit."). The outcome is a binary collapse of the 1-5
happiness scale (happy = codes 1-2, unhappy = codes 3-5). Three model specifications
run per year, 2013 through 2017:

* **Main** — perception and satisfaction based predictors: demographics, the
  satisfaction batteries (income, health, housing, relationships, leisure, safety),
  materialism, the subjective wellbeing ladder, hope, household income tier, housing
  tenure, forward-looking expectation, religiosity, and three group-level flags for
  whether any item in the negative-life-event, social-pressure, or housing-issue
  batteries fired at all.
* **Events** — objective occurrence and battery based predictors instead of
  perceptions: specific life events (went into debt, had a child, got married, lost a
  job, and so on), housing problems (heating, lighting, flooding), specific pressure
  items, crime mistreatment, and outlook items.
* **Combined** — the Main specification plus a forward-selected subset of Events terms
  that added explanatory power on top of it.

`questionnaire_mapping/ologit_questionnaire_mapping.csv` documents every raw survey
variable considered for this exercise: whether it ended up in Main, Events, or
Combined, the reference question wording, and its column name and answer options for
each year 2013-2017. Output is written to `agg_data/main.csv`, `agg_data/events.csv`,
and `agg_data/combined.csv`.

### GNH: the new 9-domain GNH index (2013-2017)

`Data Preparation_GNH 2013-7.R`, `questionnaire_mapping/swbi_gnh_domain_mapping.csv`

A rebuilt Gross National Happiness index using the full 2013-2017 questionnaire (321
variables considered, well beyond the roughly 70 items the original SWBI script used),
restricted to items that can actually be scored on a 0-1 scale, and mapped onto GNH's
domain structure using PCA-weighted averages and an Alkire-Foster-style "any issue
present" rule rather than static, pre-estimated weights. This work only covers
2013-2017: the item batteries it depends on (crime, social pressure, housing-issue,
and municipal-service-by-administration-type questions) only exist in the
questionnaire from 2013 onward.

#### Domain and sub-domain

A **domain** is one of the areas of life GNH scores separately. This project uses nine:
Psychological Wellbeing, Health, Education, Time Use, Good Governance, Community
Vitality, Ecological Diversity and Resilience, Living Standards, and Job Satisfaction.
The last one is not part of Bhutan's original nine; it replaces Cultural Diversity and
Resilience, which had no real counterpart left in the Turkish survey once its only
candidate question turned out to be an ordinary municipal-service rating rather than a
genuine measure of cultural life.

A **sub-domain** is a smaller group of related questions inside one domain. The LSS
asks several questions that all point at the same underlying thing, and a sub-domain is
where those get combined into a single number before the domain score is built. Health,
for example, splits into two sub-domains: overall health satisfaction (one question)
and problems with the healthcare system (thirteen questions about specific complaints).

#### How a sub-domain score is calculated

Every question is first rescaled onto a 0-1 range, oriented so that 1 means the best
possible answer and 0 means the worst. From there, one of five methods applies,
depending on what kind of question the sub-domain is made of.

**A single question.** If a sub-domain is just one question (own health satisfaction,
job satisfaction, housing satisfaction), the sub-domain score is that question's
rescaled value.

**PCA-weighted average.** Where several related satisfaction-type questions make up a
sub-domain (satisfaction with friends, neighbours, family, and marriage, say), a plain
average would treat every question as equally informative, which usually is not true.
Instead, the script runs Principal Component Analysis (PCA) across the group and uses
the resulting weights: a question that moves closely together with the rest of the
group gets more weight, one that is more of an outlier gets less. If someone did not
answer every question in the group, the weights are rescaled so the ones they did
answer still add up to a full weight of 1.

**Any-issue-present rule.** Some sub-domains are built from batteries of "did this
problem happen" or "did you experience this" questions: crime victimisation, social
pressure, healthcare system problems, education system problems, housing problems, and
noise problems. A plain average would let a couple of bad answers get diluted by many
good ones, which misrepresents what the group is meant to capture: whether someone's
life was free of these problems at all. The rule is stricter instead: the sub-domain
scores 1 only if every question in the group came back completely clean; a single
problem anywhere in the group brings the whole sub-domain down to 0. This mirrors the
Alkire-Foster method Bhutan's own GNH index uses at the domain level, applied here one
level down, at the sub-domain level.

**Ownership rule.** The asset ownership sub-domain asks whether a household has a
workplace, a vehicle, or owns its own home. Owning any one of the three is enough; the
sub-domain score is 1 if at least one applies, 0 if none do.

**Fixed tier mapping.** Educational attainment does not fit a 0-1 rescale in the usual
way, because "which school did you finish" is a category, not a satisfaction score. It
is mapped onto a fixed scale instead: no schooling = 0, primary school = 0.33,
secondary school = 0.66, tertiary education = 1.

#### A special case: local governance

Turkish municipalities come in two kinds: a regular municipality (belediye) or, in
areas without one, a provincial special administration (il özel idare). A resident
falls under exactly one of the two, never both, so the survey asks its
municipal-service questions twice, once worded for each kind of administration. For
every matched pair of questions (satisfaction with the water supply, say, asked once
for belediye residents and once for il özel idare residents), only one side will ever
have an answer for a given person. The script takes whichever side answered, then
treats the result as one ordinary sub-domain question like any other going into the
local governance sub-domain's PCA-weighted average, alongside a handful of
belediye-only services that il özel idare does not provide at all (fire brigade, waste
collection, and so on).

#### Domain scores

Once every sub-domain has a score, the domain score is built the same way one level up.
A domain with only one sub-domain (Psychological Wellbeing, Time Use, Job Satisfaction)
takes that sub-domain's score directly. A domain with several sub-domains combines them
with the same PCA-weighted average described above, whatever mix of sub-domain types
(single-question, any-issue, PCA-averaged) they happen to be built from.

#### The overall GNH score

The final GNH score combines all nine domain scores the same way again: a PCA-weighted
average across them. It sits on the same 0-1 scale as every question and sub-domain
score underneath it, 1 meaning the best possible outcome across every domain, 0 the
worst.

#### Weighting note

Every score above is calculated per respondent. Turning these into a single national
or yearly figure requires the survey's own sampling weight, which is what
`Data Preparation_GNH 2013-7.R`'s final summary step applies (`weighted.mean(..., weight)`)
to produce one row per year, written to `agg_data/gnh_turkey_2013_2017.csv`.
