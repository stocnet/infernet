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

  regress a network’s ties on other ties and/or nodal or dyadic
attributes

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
more male than female superheros, as well as clearly more superheros of
mutant origin than others. So what might seem like homophily could just
be a result of there being many more opportunities for ties between
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
[`test_random()`](https://stocnet.github.io/infernet/reference/tests.md).
Plot the results of running this function with respect to the EI index
on each of the three variables. These plots can be made if you have
[autograph](https://stocnet.github.io/autograph/) installed and loaded.
You can specify that one thousand simulations should be used using
`times = 1000`.

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

Let’s start with a pretty maximally specified model (note that it
doesn’t make sense to include both ego and alter effects because these
networks are undirected). We are using `times = 200` here because of
time-out limitations in the `learnr` tutorial system, but for
publication quality results you would want to base your conclusions on
2000 simulations or more.

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

This violin plot presents the distribution of coefficients from
permutations of the network, with the coefficient fitted from the data
as a red dot. Lines are used to indicate 95% thresholds, but here the
distributions are rendered so wide that they are often not visible.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Free play

While these are the conclusions from this ‘play’ data, you may have more
and more interesting data at hand. How would you go about specifying
such a model? Why is such an approach more appropriate for network data
than linear or logistic regression?

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
- Ego : An ego is a focal node in a network.
- Heterogeneity : A measure of how varied the nodes in a network are.
- Heterophily : A tendency for nodes to connect to dissimilar nodes.
- Homophily : A tendency for nodes to connect to similar nodes.
- Man : A random network conditional on the number of mutual,
  asymmetric, and null dyads.
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
