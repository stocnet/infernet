# Diversity and Regression

**Static preview**: This is a static, read-only preview of the
“Diversity and Regression” tutorial. To keep it a preview, the code
output and exercise solutions are not shown here, and the quizzes are
replaced with notes like this one. Install the package and run
`run_tute()` at the R console to work through the tutorial interactively
— running the code, seeing the results, and getting hints, solutions,
and quizzes.

## This tutorial

![gif of deadpool
greeting](https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExeDFidmtrcTJhenU1NGRtdXA5N292Ynp3Y3BxcHRhYjR2MnowOGJ0ZiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/djRJNZqj508sE/giphy.gif)

Within a network, nodes and ties vary. But how much do they vary and are
there associations in this variance? By the end of this tutorial, you
will be able to:

  measure the richness of a network in terms of attribute types

  measure how diverse a network is

  measure how heterophilous the ties in a network are

  test how heterophilous the ties in a network are compared to random
networks

  test how heterophilous the ties in a network are compared to
permutations of the original network

  regress a network’s ties on nodal and dyadic attributes

  choose which categories of an attribute enter a regression

  account for differences among senders and receivers with fixed effects
or random intercepts

  regress one layer of a network on other layers, including in a
cognitive social structure

  move a regression’s formula to another model, and check what it keeps

## Initial visualisation

For this session, we’ll explore a couple of different datasets. First,
let’s examine homogeneity/ heterogeneity in the Marvel dataset from
[manynet](https://stocnet.github.io/manynet/), `fict_marvel`. The
dataset is quite complicated, so to make this simpler, let’s concentrate
on:

- just the relationships part of the network
- just the positive (friendship) ties and not the negative (enmity) ties
- the main (giant) component without any isolates
- just those characters that appear in the comics more than average

Fortunately, all these data cleaning moves are easy to do in
[manynet](https://stocnet.github.io/manynet/), and can be seen in the
following chunk in order:

This gives us an undirected network of nearly twenty characters and a
little more than 100 edges. Recall that this data has several nodal
attributes. Let’s explore a couple of these attributes, “Gender” and
“PowerOrigin”, visually using `graphr()`.

These variables seem to be distributed unevenly across the network.
There seems to be some homophily – or like choosing like – operating
here, but it is difficult to say conclusively because there are clearly
more male than female superheroes, as well as clearly more superheroes
of mutant origin than others. So what might seem like homophily could
just be a result of there being many more opportunities for ties between
nodes of some categories. We therefore need to establish how diverse
this network really is.

## Measuring richness

Some categorical variables have more categories than others, and this
can be thought of as indicating greater diversity than e.g. a binary
categorical variable. A measure of this ‘richness’ is inherited from the
study of biodiversity. Richness calculates the number of different
categories that appear in the data for a given variable. Let’s calculate
the richness of all of the variables in our Marvel data.

``` r

net_node_attributes(marvel_friends)
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

Note though that ‘richness’ as a network measure does not include any
sense of how distributed these categories are around the network. There
is a measure of nodal richness available, which counts the number of
different categories to which each node is tied, but this does not offer
a summary of how evenly distributed the appearance of categories are.
For that we would need to move on to something like the Blau index.

## Measuring diversity

Another measure that reflects the diversity or heterogeneity in the
network for each attribute is the Blau Index. Recall that the Blau index
for any given variable is:

``` math
1 - \sum p_i^2
```

where $`p_i`$ represents the proportion of observations holding $`i`$,
which indexes each of the given categories. A perfectly homogeneous
group (all the same category) would receive a score of 0, while a
perfectly heterogeneous group (with members spread evenly over the
maximum categories) would receive a score of 1. Obtain the network
diversity scores for the five attributes used above.

Looks like there is more diversity in terms of where these characters
got their powers, whether they have significant intellectual powers, and
their gender, than their attractiveness or their wealth.

We can also cross-reference this diversity. For example, we might be
interested in whether our comic book heroes are equally gender diverse
across their (power) origin stories, or equally intellectually diverse
across gender.[^1]

[`to_subgraphs()`](https://stocnet.github.io/manynet/reference/modif_split.html)
splits the network into one subgraph for each category, so the length of
the vector returned as a result is the number of categories in the
attribute that is split on. It looks like some origin stories are more
gender diverse than others. Gods (just Thor here) and humans are all
men, whereas those with mutant or radiation origin stories are more
gender diverse. There doesn’t appear to be much difference in intellect
across gender categories in this data.

Ok, this tells us about how (un)even the distribution of these variables
is in this network, but it doesn’t necessarily tell us whether ties are
appearing more frequently *between* nodes of similar (or different)
categories. For that we need to look at homophily / heterophily.

## Measuring heterophily

The EI (or E-I) index offers a way to measure the degree to which ties
appear between rather than within groups of nodes of the same category.
Recall that the EI index is calculated as:

``` math
\frac{E-I}{E+I}
```

where $`E`$ is the number of ties present between a variable’s
categories (i.e. external), and $`I`$ is the number of ties present
within a variable’s categories (i.e. internal). As such, an EI index of
-1 suggests perfect homophily, whereas an EI index of +1 suggests
perfect heterophily. (This is why the function is called
[`net_by_heterophily()`](https://stocnet.github.io/netrics/reference/measure_assort_net.html)).

Check how homophilic three variables in the network are, “Gender”,
“PowerOrigin”, and “Attractive”. Please assign the results so that we
can use them later.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

Ultimately though, these are just scores, and doesn’t tell us whether
this is any more or less than what we might expect the score to be by
chance for a network of this size and density and distribution of that
attribute.

## CUG tests

To see whether we should be surprised by scores this high/low, we
compare these scores with those from a series of random graphs
(Erdös-Renyi/Bernoulli) of the same dimensions and distribution of the
attribute. This can help us establish whether there is more homophily or
heterophily than expected by chance.

This is often called a conditional uniform graph or CUG test, but
[infernet](https://stocnet.github.io/infernet/) uses more descriptive
function names, such as
[`test_random()`](https://stocnet.github.io/infernet/reference/test_measures.md).
Plot the results of running this function with respect to the EI index
on each of the three variables. These plots can be made if you have
[autograph](https://stocnet.github.io/autograph/) installed and loaded.
You can specify that five hundred simulations should be used using
`times = 500`.

The plots of these results use a dotted vertical line for 0 where this
is in bounds, a red vertical line for the observed score, and a density
plot of the scores from the randomly generated networks. The grey tails
of the distribution are a visual aid indicating the most extreme 5% of
scores from the distribution. The results are *really* interesting.

![gif of ironman
explosion](https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExNzlsM3J1dm9tb2tlYjB4bGw0cGxuc3RodXpvdjNhcGJ5cXV2MndjbiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/AbYxDs20DECQw/giphy.gif)

Despite being the larger coefficients (in absolute terms), it looks like
we cannot reject the null hypothesis that there is no homophily for
gender nor for attractiveness. Both observed scores fall within the
range of scores we would expect from randomly generated networks with
the same distribution of that variable.

However, we can reject the null hypothesis with respect to their power
origin story. While the coefficient itself is close to 0 (neither strong
homophily nor heterophily), all the random networks generated returned
larger EI scores, between .1 and .4.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## QAP tests

Ah, but perhaps random graphs are not the best reference group for
establishing whether there is a significant homophily effect here. After
all, social networks are not completely random; they are *structured* in
particular ways, such as some nodes having higher degrees than others,
or there being a core and periphery or community topology.

Another approach to establishing a baseline for whether we should be
surprised by a given score or not is to use permutations of the
underlying network instead of random graphs. Permuting the network
retains the structure of the network because the ties are kept and only
the labels (variables) are reassigned randomly. Let’s first plot the
observed data and some permuted data next to each other.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

This single permutation suggests a more even mixing of these attributes
is possible, but it is just a single permutation. Let’s try a QAP test
that runs this over a succession of permutations, just as we did with
random graphs. Plot the results for gender and power according to the
random and permutation baselines.

Again, we see that there is perhaps nothing so surprising that we got
the homophily score for gender that we did, but the lack of power origin
heterophily is surprising. Note how the distributions are generally
wider when permuting the observed network than creating a random
distribution (be mindful of the scale of the *x*-axis). That is, taking
into account the structure of the network leads us to expect a larger
spread in the EI index than when the variable is distributed across a
random network.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

CUG and QAP tests are useful for establishing whether there is more or
less homophily than expected by chance, with QAP tests being more
conservative and better taking into account the structure of the
network. Alternative reference networks can also be used that condition
on more structural information than completely random graphs, but
introduce potentially more variation in the network structure than
permutations. These include for example configuration networks that
retain the degree distribution observed in the original network, but
could result in varying clustering, or networks that are conditioned on
the dyad census (i.e.  MAN models). And yet none of these approaches
allow us to examine multiple variables at once or control for other
factors. For that we need to move on to network regression.

## Network regression

Next let us examine homophily in another network. The data were
collected as part of an early experiment on communication between social
network researchers who were using an Electronic Information Exchange
System (EIES). You may recognise some of the names. The main network
consists of 32 scholars with directed ties weighted by the total number
of messages sent from $`i`$ to $`j`$ over the period of the study. Nodal
attributes collected include the primary discipline and number of
citations in the social science citation index at the start of the
study.

``` r

ison_networkers
graphr(ison_networkers, node_color = "Discipline")
```

Let’s use both the continuous `Citations` and the categorical
`Discipline` variables and come up with a couple of key hypotheses:

- $`H_1`$: researchers send more emails to those who are cited more
  (popularity hypothesis)
- $`H_2`$: researchers send more emails to those of the same discipline
  (disciplinary homophily hypothesis)

[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
fits a MRQAP model from a formula. The left-hand side names the ties to
explain, here the `weight` of each tie, and the right-hand side lists
the terms that might explain them:

- `ego(Citations)` and `alter(Citations)` are the sender’s and the
  receiver’s citations. Because this network is directed , both ego and
  alter effects make sense. $`H_1`$ is about the alter effect.
- `diff(Citations)` is the absolute difference between the sender’s and
  the receiver’s citations. `diff(Citations, transformer_fn = identity)`
  gives the signed difference instead.
- `ego(Discipline)` and `alter(Discipline)` are the sender’s and the
  receiver’s discipline. A categorical attribute gets one 0/1 column for
  each category, less the first category (here Anthropology), which is
  the *reference*.
- `same(Discipline)` is 1 where the sender and the receiver share a
  discipline. This is $`H_2`$.

Let’s start with a model of all five terms for each attribute. Homophily
is only interpretable net of the effects it contains, since two
researchers who are both highly cited are alike, but each is also highly
cited. We are using `times = 200` here so that the model runs quickly in
this tutorial, but for publication quality results you would want to
base your conclusions on 2000 permutations or more.

By default, the printing method for this model includes the key
information you might need. We can also use tidy methods to obtain just
the table of coefficients
(e.g. [`tidy()`](https://generics.r-lib.org/reference/tidy.html)) or the
model summary statistics
(e.g. [`glance()`](https://generics.r-lib.org/reference/glance.html)).
[autograph](https://stocnet.github.io/autograph/) also includes a plot
method for these results to facilitate the quick interpretation of these
results, and as long as this is loaded you can simply call
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the model
object.

This violin plot presents the distribution of the test statistic for
each coefficient across the permutations of the network, with the
statistic fitted from the data as a red dot. Lines inside each violin
mark the 95% thresholds. A red dot outside those lines is one that the
permutations rarely reach.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Categories

`same(Discipline)` gives one coefficient for all disciplines, so it
assumes that sociologists are as homophilous as anthropologists. That
need not be so. Two terms split a categorical attribute further:

- `same(Discipline, diff = TRUE)` gives one 0/1 column for each
  discipline, 1 where the sender and the receiver both hold it.
- `mix(Discipline)` gives one 0/1 column for each pair of a sender’s and
  a receiver’s discipline, such as sociologist to anthropologist, less
  the first pair as the reference.

With four disciplines, `mix()` can quickly use up a small network.
`levels =` chooses which categories (or pairs) get a column. Give their
names, as they appear in the results, such as `levels = "Sociology"` or,
for `mix()`, `levels = "Sociology -> Anthropology"`. The categories that
are left out become the reference together.

Fit a model with the citation terms, discipline homophily for each
discipline, and the two mixing terms between sociologists and
anthropologists, in both directions.

Now see how `levels =` changes the reference category of `ego()` and
`alter()`. Fit a model that asks only whether sociologists send or
receive more messages than everyone else, and whether researchers of the
same discipline message each other more.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Sender and receiver effects

Some researchers send many more messages than others, and some receive
many more. The ties of one sender are then not independent of one
another, and a model that ignores this may mistake a busy sender for an
effect of their attributes. There are two ways to account for each
node’s own level of activity and popularity.

***Fixed effects*** absorb a separate intercept for each sender and each
receiver. Write them after a bare bar at the end of the formula, as in
[fixest](https://lrberge.github.io/fixest/): `| sender + receiver`.
Because these intercepts absorb everything about a sender or a receiver,
a term that does not vary within them, such as `ego(Citations)` or
`alter(Discipline)`, cannot be estimated, and the model stops and names
it. Only the dyadic terms, which vary within each sender and receiver,
can stay. The model then reports a ‘within’ R-squared, the share of the
variance within senders and receivers that it explains.

Fit a model of citation difference and discipline homophily with sender
and receiver fixed effects.

***Random intercepts*** instead treat each sender’s and each receiver’s
intercept as drawn from a normal distribution, as in
[lme4](https://github.com/lme4/lme4/). Write each one in parentheses:
`(1 | sender) + (1 | receiver)`. Random intercepts keep the nodal terms
in the model, but each permutation now fits a mixed model, which takes
longer. Here we use `times = 100`.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Model controls

So far each model has used its default controls.
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
shows them all, and checks any that you change. Its result goes to the
`control =` argument of
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).
The most useful controls are:

- `family`: `"auto"` reads it from the outcome (`"binomial"` for a
  binary outcome, otherwise `"gaussian"`). The message counts here might
  also be fitted as `"poisson"` or `"negbin"`.
- `permute`: `"predictor"` (Dekker’s double semi-partialling, the
  default) or `"outcome"`.
- `seed`: a number, so that the same permutations, and so the same
  p-values, come back each time.
- `ncores`: the number of cores to run the permutations on.

Look at the controls, then fit a model with a seed, twice.

Running in parallel only pays where each permutation is costly: a family
other than gaussian, random intercepts, or a cognitive social structure
(see the next section), on a network of more than about 50 nodes, with
1,000 or more permutations. Elsewhere, starting the workers costs more
time than it saves. For such a model, outside this tutorial, you would
write, for example:

``` r

estimate_mrqap(advice ~ tie(friends) + same(dept) + (1 | reporter),
               ison_hightech, times = 2000,
               control = control_mrqap(ncores = 2, seed = 1234))
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Layers and perceptions

The networks so far have had one kind of tie. A multiplex network has
several, called layers, and one layer can explain another.
`ison_hightech` holds Krackhardt’s (1987) data on 21 managers of a
high-tech company. Every manager reported who goes to whom for advice,
and who is friends with whom. Each of those layers is a *cognitive
social structure* (CSS): 21 reports of the whole network, one from each
manager. The `reports` layer is the formal reporting structure, which is
the same for everyone.

``` r

ison_hightech
```

[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
detects a CSS and fits every reported dyad , one for each sender,
receiver, and reporter: 21 x 20 x 21 = 8,820 observations. A permutation
relabels senders, receivers, and reporters together. These terms read
another layer:

- `tie(friends)` is whether the sender is friends with the receiver, in
  the same reporter’s report, since `friends` is a CSS layer too.
- `exchange(friends)` is the reverse tie, whether the receiver is
  friends with the sender.
- `indeg(reports, node = "ego")` is the sender’s indegree in the
  reporting structure, that is, how many managers report to them.
  `node = "alter"` would read the receiver’s instead.

A term must not read the outcome’s own ties, since permuting the outcome
would not permute such a predictor with it. So `tie(advice)` would stop
with an error.

Fit a model of advice on friendship, reciprocated friendship, the
sender’s number of reports, and being in the same department. Advice
ties are binary, so this is a logistic model of 8,820 observations, and
we use `times = 50` with a seed.

A report about one’s own ties can differ from a report about the ties of
others. The *reporter* terms describe who made each report:

- `reporter(level)` is the reporter’s level in the hierarchy, the same
  over their whole report.
- `reporter_ego` is 1 where the reporter is the sender, that is, in a
  report about whom the reporter goes to for advice.
- `reporter_alter` is 1 where the reporter is the receiver.

Add the reporter’s level and `reporter_ego` to the model.

Each manager may also simply report more or fewer ties overall.
`(1 | reporter)` adds a random intercept for each reporter, and
`| reporter` a fixed effect, which absorbs `reporter(level)`. On a CSS,
each permutation of such a model takes much longer, so try these outside
the tutorial, with `ncores = 2` and more permutations.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Moving a formula

The same formula can describe other models of the same network.
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
rewrites an MRQAP formula for [ergm](https://statnet.org), and
[goldfish](https://stocnet.github.io/goldfish/)’s own `specify_dynam()`
for a DyNAM. Each term that has an equivalent is rewritten, such as
`diff(Citations)` as `absdiff("Citations")` and a `| sender` fixed
effect as ergm’s `sender` term, and each term that the other model
cannot fit is dropped and named. Give the network as well, so that a
categorical attribute is written as a categorical term.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) of the result
has one row for each term, with what the other model makes of it.

Rewrite a model of `ison_networkers` for ergm, and check each term. Then
do the same, without the network, for a model of `ison_hightech`.

The result is an ordinary formula, so
[`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html) could fit it as
it is. We do not fit it here, since an ERGM takes longer to estimate
than this tutorial allows, and an ERGM of the message counts would need
a valued ERGM.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Free play

While these are the conclusions from this ‘play’ data, you may have more
and more interesting data at hand. How would you go about specifying
such a model? Why is such an approach more appropriate for network data
than linear or logistic regression?

If you want another network to try, `ison_lawfirm` holds Lazega’s data
on the advice, friendship, and cowork ties among 71 lawyers, with their
status, gender, office, practice, and seniority. Does advice follow
friendship, office, or practice? Which terms would you hold fixed for
each sender?

## Glossary

Here are some of the terms that we have covered in this module:

- Alter : An alter is a node connected to a focal node (ego).
- Community : A community is a set of nodes more densely connected to
  one another than to other nodes in the network.
- Configuration : A configuration network is one where the degree
  sequence is fixed.
- Core : A core-periphery is a bipartition of nodes into maximally dense
  and sparse blocks.
- Cug : A conditional uniform graph (CUG) is a random graph generated
  under constraints defined by some property of the observed network.
- Directed : A directed network is a network where the ties have a
  direction, from a sender to a receiver.
- Dyad : A dyad is a pair of nodes and the ties between them.
- Ego : An ego is a focal node in a network.
- Heterogeneity : A measure of how varied the nodes in a network are.
- Heterophily : A tendency for nodes to connect to dissimilar nodes.
- Homophily : A tendency for nodes to connect to similar nodes.
- Man : A random network conditional on the number of mutual,
  asymmetric, and null dyads.
- Mrqap : A multiple regression quadratic assignment procedure (MRQAP)
  is a regression model for network data using a permutation test.
- Multiplex : A network that includes multiple types of tie.
- Permutation : A permuted network is one where the nodes/ties have been
  rearranged or resequenced.
- Qap : A quadratic assignment procedure (QAP) is a permutation test for
  network data.
- Random : A random network is one where ties are placed among nodes
  according to some probability distribution.
- Richness : A measure of how many different types of nodes are in a
  network.
- Undirected : An undirected or line network is one in which tie
  direction is undefined.

[^1]: Note that this works for calculated categorical variables too,
    such as cluster/group assignment from community detection or
    equivalence classes.
