# GNH Terminology

This note explains the terms used in `questionnaire_mapping/swbi_gnh_domain_mapping.csv`
and in `Data Preparation_GNH 2013-7.R`: what a domain and a sub-domain are, and how each
sub-domain score and domain score is calculated. It covers 2013-2017 only, because the
question batteries this scoring depends on (crime, social pressure, housing problems,
municipal service ratings split by administration type) only exist in the survey from
2013 onward. Earlier years cannot be scored with this method.

## What GNH is

Gross National Happiness (GNH) is Bhutan's national wellbeing framework. Instead of
judging a country's progress by income alone, it scores people's lives across nine
areas of life, called domains, and combines them into one index. This project reuses
that idea to score Turkey's Life Satisfaction Survey (LSS, TÜİK) the same way, since the
LSS asks many of the same kinds of questions Bhutan's own survey does.

## Domain and sub-domain

A **domain** is one of the nine areas of life GNH scores separately: Psychological
Wellbeing, Health, Education, Time Use, Good Governance, Community Vitality, Ecological
Diversity and Resilience, Living Standards, and Job Satisfaction. The last one is not
part of Bhutan's original nine; it replaces Cultural Diversity and Resilience, which had
no real counterpart left in the Turkish survey once its only candidate question turned
out to be an ordinary municipal-service rating rather than a genuine measure of cultural
life (see the mapping generator's changelog for that decision).

A **sub-domain** is a smaller group of related questions inside one domain. The LSS asks
several questions that all point at the same underlying thing, and a sub-domain is where
those get combined into a single number before the domain score is built. Health, for
example, splits into two sub-domains: overall health satisfaction (one question) and
problems with the healthcare system (thirteen questions about specific complaints).

## How a sub-domain score is calculated

Every question is first rescaled onto a 0-1 range, always oriented so that 1 means the
best possible answer and 0 means the worst. From there, one of five methods applies,
depending on what kind of question the sub-domain is made of.

**A single question.** If a sub-domain is just one question (own health satisfaction,
job satisfaction, housing satisfaction), the sub-domain score is that question's
rescaled value, with nothing else to combine it with.

**PCA-weighted average.** Where several related satisfaction-type questions make up a
sub-domain (say, satisfaction with friends, neighbours, family, and marriage), a plain
average would treat every question as equally informative, which usually is not true.
Instead, the script runs Principal Component Analysis (PCA) across the group and uses
the resulting weights: a question that moves closely together with the rest of the group
gets more weight, a question that is more of an outlier gets less. If someone did not
answer every question in the group, the weights are rescaled so the ones they did answer
still add up to a full weight of 1.

**Any-issue-present rule.** Some sub-domains are built from batteries of "did this
problem happen" or "did you experience this" questions: crime victimisation, social
pressure, healthcare system problems, education system problems, housing problems, and
noise problems. For these, a plain average would let a couple of bad answers get diluted
by many good ones, which misrepresents what the group is actually measuring: whether
someone's life was free of these problems at all. So the rule is stricter: the
sub-domain scores 1 only if every question in the group came back completely clean; a
single problem anywhere in the group brings the whole sub-domain down to 0. This mirrors
the Alkire-Foster method Bhutan's own GNH index uses at the domain level, applied here
one level down, at the sub-domain level.

**Ownership rule.** The asset ownership sub-domain asks whether a household has a
workplace, a vehicle, or owns its own home. Owning any one of the three is enough; the
sub-domain score is 1 if at least one applies, 0 if none do.

**Fixed tier mapping.** Educational attainment does not fit a 0-1 rescale in the usual
way, because "which school did you finish" is a category, not a satisfaction score. It
is mapped onto a fixed scale instead: no schooling = 0, primary school = 0.33, secondary
school = 0.66, tertiary education = 1.

## A special case: local governance

Turkish municipalities come in two kinds: a regular municipality (belediye) or, in areas
without one, a provincial special administration (il özel idare). A resident falls under
exactly one of the two, never both, so the survey asks its municipal-service questions
twice, once worded for each kind of administration. For every matched pair of questions
(say, satisfaction with the water supply, asked once for belediye residents and once for
il özel idare residents), only one side will ever have an answer for a given person. The
script takes whichever side answered, then treats the result as one ordinary sub-domain
question like any other going into the local governance sub-domain's PCA-weighted
average, alongside a handful of belediye-only services that il özel idare does not
provide at all (fire brigade, waste collection, and so on).

## Domain scores

Once every sub-domain has a score, the domain score is built the same way one level up.
A domain with only one sub-domain (Psychological Wellbeing, Time Use, Job Satisfaction)
just takes that sub-domain's score directly. A domain with several sub-domains combines
them with the same PCA-weighted average described above, whatever mix of sub-domain
types (single-question, any-issue, PCA-averaged) they happen to be built from.

## The overall GNH score

The final GNH score combines all nine domain scores the same way again: a PCA-weighted
average across them. It sits on the same 0-1 scale as every question and sub-domain
score underneath it, 1 meaning the best possible outcome across every domain, 0 the
worst.

## Weighting note

Every score above is calculated per respondent. Turning these into a single national or
yearly figure requires the survey's own sampling weight, which is what
`Data Preparation_GNH 2013-7.R`'s final summary step applies (`weighted.mean(..., weight)`)
when it produces one row per year.
