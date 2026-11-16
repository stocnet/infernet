# Opinion and Learning

**Static preview**: This is a static, read-only preview of the “Opinion
and Learning” tutorial. To keep it a preview, the code output and
exercise solutions are not shown here, and the quizzes are replaced with
notes like this one. Install the package and run `run_tute()` at the R
console to work through the tutorial interactively — running the code,
seeing the results, and getting hints, solutions, and quizzes.

## This tutorial

The “Diffusion and Contagion” tutorial follows states that nodes adopt
or not, such as an infection or an innovation. This tutorial follows
opinions that are numbers, such as the probability that someone puts on
a statement being true, or their position on a scale from left to right.
Here a node does not adopt an opinion from one neighbour: it moves its
opinion towards the opinions of the nodes it listens to.

![learn every day
gif](https://media.giphy.com/media/v1.Y2lkPWVjZjA1ZTQ3bXdtbGs1azJnZWUzdGZycTRsMGg0a2twM2RxYjdveWtuNzV3eWo1MiZlcD12MV9naWZzX3NlYXJjaCZjdD1n/xjQfDCSRr2jkH3SPab/giphy.gif)

By the end of this tutorial, you will be able to:

  ‘play’ a DeGroot learning model over a network, and plot the result

  say when a network will reach a consensus, and how fast

  understand how stubbornness keeps disagreement alive

  understand bounded confidence, and the clusters of opinion it makes

  see how maximally diverse groups learn differently from groups of like
minds

  understand repulsion, and how a balanced signed network polarises

  find the nodes with the most influence on the opinions at the end

  say when a crowd stays wise, when talking makes it wiser, and where
misinformation does most harm

  show how a zealot can move the window of opinions that others will
listen to

  tell the equilibria of these models apart, and when there is none

## Learning models

The simplest learning model is the DeGroot model. At each step, every
node takes the average of the opinions of the nodes it listens to. A tie
from one node to another means that the first node listens to the
second. A node divides its attention among the nodes it listens to, in
proportion to the tie weights (their absolute values, for a signed
network).
[`to_influence()`](https://stocnet.github.io/manynet/reference/modif_influence.html)
shows these shares as a matrix. Under DeGroot, a node does not count its
own opinion, unless the network has a tie from the node to itself. All
nodes update at once at each step, and the ties do not change, whatever
the opinions do. A question often asked of these models is whether, from
different first opinions, the opinions converge through (network)
interaction. A network that is strongly connected and aperiodic
converges to a consensus of (any) opinions entered.

### Expectations of convergence and consensus

Let’s try this out on the `ison_networkers` dataset of communication
among network academics. Perhaps we wish to get them to change their
local networks/behaviour, but they have different beliefs about whether
this will make a difference. To see whether this network will converge
to consensus, check whether the network is connected and aperiodic :

- `is_connected()` marks whether every node can reach every other node.
  On a directed network, it follows the direction of the ties by default
  (strong connectivity); add `connectivity = "weak"` to ignore their
  direction.
- [`is_aperiodic()`](https://stocnet.github.io/manynet/reference/mark_features.html)
  marks whether network is aperiodic, meaning there is no integer k \> 1
  that divides the length of every cycle of the graph.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### Playing the DeGroot learning model

Now let’s see whether you are right. We want to see whether some random
distribution of beliefs converges to a consensus in this network
(`ison_networkers`). Let’s play the DeGroot learning model on this
network with a vector of random beliefs (the same length as the nodes in
the network) drawn from the binomial distribution with **probability
0.25**. Create the distribution of **beliefs** and graph the network to
show where they have been distributed. Then play the learning model with
[`play_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html)
and these beliefs, and plot the result.

Each line in this plot represents the belief trajectory of a single node
at each step. About a quarter of the nodes begin believing, and the
other three quarters do not. Then we can see how responsive these nodes
are to the random distribution of beliefs across the network. Some
revise their beliefs more significantly than others. To see the opinion
of each node at each step as a table, use
[`as_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html).
[`play_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html)
stops when no opinion moves by more than `tolerance`, so the last step
is very near to the end, but not exactly at it.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### How fast is a consensus?

Whether a network reaches a consensus is one question; how fast it
reaches it is another.
[`net_by_convergence()`](https://stocnet.github.io/netrics/reference/measure_opinion.html)
from [netrics](https://stocnet.github.io/netrics/) measures the share of
the distance to the opinions at the end that is closed at each step, in
the long run. A value of 1 settles in one step, and a value of 0 never
settles. It needs only the network, not a played model. Compare the
networkers with a complete network of the same size (32 nodes), and with
two rings: one of 31 nodes and one of 32 nodes.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Stubbornness

In the DeGroot model, a strongly connected, aperiodic network always
ends in a consensus. But many disagreements last. One reason is that
people hold on to their first opinion. In the Friedkin–Johnsen model,
each node has a stubbornness from 0 to 1: how much it keeps its first
opinion, compared with what the network says. A stubbornness of 0 is
DeGroot’s model, and a stubbornness of 1 is a node that never moves.
Note that a stubborn node is pulled back to its *first* opinion, not to
the opinion it held at the last step. That first opinion never changes.

### A path of three

Let us make the smallest example: a path A–B–C, where A starts at 0, B
at 0.5, and C at 1.
[`create_explicit()`](https://stocnet.github.io/manynet/reference/make_explicit.html)
makes the path from a formula. Play the opinions with no stubbornness,
and then with a `stubbornness` of 0.5 for every node. Use
[`as_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html)
to see the opinions at each step.

``` r

path <- create_explicit(A-B-C)
graphr(path)
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

Note that with stubbornness the opinions still settle, but they settle
apart. Partly stubborn nodes are enough to keep a disagreement alive.

### Stubborn networkers

Now let only the networker with the highest eigenvector centrality be
stubborn, with a stubbornness of 1, and start them at an opinion of 1
while all others start at 0. Then let all networkers be a little
stubborn (0.1), and compare the two plots.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Bounded confidence

Another reason that disagreements last is that people listen only to
those whose opinions are near enough to their own. (If you have ever
left a dinner party early, you know this model.) These are models of
bounded confidence . `confidence` says how far apart two opinions can be
for one node still to listen to the other. Two rules use it:

- `rule = "hegselmann_krause"`: all nodes at once take the average of
  the opinions of only those nodes they listen to whose opinions are
  within `confidence` of their own. Unlike DeGroot, each node always
  counts its own opinion here. This rule is deterministic.
- `rule = "deffuant"`: at each step, one node is drawn at random, and it
  draws one of the nodes it listens to. If their opinions are within
  `confidence`, each moves halfway towards the other. This rule is
  random, and it changes only one pair at each step, so it needs many
  more `steps`.

### Clusters of opinion

Draw the first opinions of the networkers at random, from 0 to 1. Play
the Hegselmann–Krause rule with a `confidence` of 0.1, and plot it.

Then see how the clusters change with the `confidence`.
[`sweep_opinion()`](https://stocnet.github.io/infernet/reference/sweep.md)
plays the rule for each setting that you give it in `vary`, here for
each `confidence` from 0.05 to 0.5, and counts the clusters of opinion
at the end of each. Give it 100 `steps`, so that the opinions have time
to settle.

``` r

set.seed(1)
first <- runif(net_nodes(ison_networkers))
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### Random encounters

Think of the Deffuant rule as a series of coffee breaks: two colleagues
meet, and if they are not too far apart, they meet in the middle. Now
play the Deffuant rule on the same first opinions, with a `confidence`
of 0.1 and 2000 `steps`. This rule is random, so each time it is played,
its result can differ. So sweep it 10 times for each `confidence` from
0.1 to 0.5, and set a seed in `control` so that you can repeat the
result.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### Diverse groups

Bounded confidence also matters for how we make groups, whether
committees, seminar groups, or juries.
[`node_in_roulette()`](https://stocnet.github.io/netrics/reference/member_cliques.html)
from [netrics](https://stocnet.github.io/netrics/) puts the nodes into
groups that are as diverse as possible on an attribute, such as their
first opinions. Let us compare such maximally diverse groups with groups
of like minds, where each group holds the nodes whose first opinions are
nearest to one another. In each group, every member listens to every
other member, which
[`create_components()`](https://stocnet.github.io/manynet/reference/make_create.html)
makes from a membership.

``` r

set.seed(1)
first <- runif(net_nodes(ison_networkers))
diverse <- node_in_roulette(ison_networkers, groups = 4,
                            attribute = first, balance = 1)
alike <- ceiling(rank(first) / 8)
diverse_groups <- create_components(32, membership = diverse)
alike_groups <- create_components(32, membership = alike)
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

Diversity spreads the information that each group holds, but it helps
only where members listen to opinions far from their own.

## Repulsion

So far, every tie pulls a node towards the opinions of the nodes it
listens to. But we may also move away from the opinions of those we
dislike: if *they* are for it, I must be against it. In a signed
network, a negative tie can stand for such repulsion: the node moves
towards the opposite of the other node’s opinion. Opposite opinions are
then opposite signs, so the opinions are on a scale centred on 0, from
-1 to 1.

A signed network is structurally balanced where its nodes split into two
camps, with positive ties inside each camp and negative ties between
them. Where it is balanced, repulsion splits the opinions into two camps
of opposite signs. Where it is not, the opinions are pulled towards 0.

### The road to war

`irps_wwi` holds the alliances (positive) and enmities (negative) of six
European powers from 1872 to 1918.
[`to_time()`](https://stocnet.github.io/manynet/reference/modif_scope.html)
gives the network in one year. Play the same first opinions on the
network in 1890 and in 1914, and check the balance of each with
[`net_by_balance()`](https://stocnet.github.io/netrics/reference/measure_features.html).

``` r

first <- c(0.5, 0.2, 0.1, -0.1, -0.3, 0.4)
graphr(to_time(irps_wwi, 1914))
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

Note that only the DeGroot rule can yet use negative ties.

## Who has influence?

Where the opinions reach a consensus, the consensus is a weighted mean
of the first opinions. The weight of each node is its influence:
[`node_by_influence()`](https://stocnet.github.io/netrics/reference/measure_central_eigen.html)
from [netrics](https://stocnet.github.io/netrics/) measures it. Without
stubbornness, it is the left eigenvector of the influence matrix from
[`to_influence()`](https://stocnet.github.io/manynet/reference/modif_influence.html),
where each row says whom a node listens to. This is also called the
social power of a node.
[`node_by_influence()`](https://stocnet.github.io/netrics/reference/measure_central_eigen.html)
and
[`node_by_opinion()`](https://stocnet.github.io/netrics/reference/measure_opinion.html)
compute the exact end, not a played one, so they need no `steps`. If the
network is periodic, the opinions swing for ever, and these functions
give the mean of the swing.

### Influence and eigenvector centrality

It is often said that the most influential nodes in a network are those
with the highest eigenvector centrality. Compare
[`node_by_influence()`](https://stocnet.github.io/netrics/reference/measure_central_eigen.html)
with
[`node_by_eigenvector()`](https://stocnet.github.io/netrics/reference/measure_central_eigen.html)
for the networkers.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### Free play: Networkers

Which networkers would you try to convince first, so that the whole
network comes to agree with you? Give an opinion of 1 to the node with
the highest influence and 0 to all others, play the DeGroot model, and
compare the consensus with its influence share,
`node_by_influence(ison_networkers, normalized = FALSE)`.

## Wisdom of crowds

Imagine that each networker makes an estimate of some true value, for
example the number of papers in a journal, with some error. The mean of
all their estimates is often close to the truth, since the errors cancel
out: the wisdom of crowds . Francis Galton found that the crowd at a
country fair guessed the weight of an ox to within one percent, better
than most of the experts there. But what if they talk first, and then
agree on one consensus? The consensus is the mean of their first
estimates, weighted by the influence of each node. Such a crowd stays
wise only where no node has too much influence.

### Does talking make the crowd wiser?

Let the true value be 50, and let each estimate miss it by an error with
a standard deviation of 10. The `normalized = FALSE` influence of each
node is its weight in the consensus, so `sum(w * errors)` is the error
of the consensus. Draw the errors 1000 times for the networkers, a star,
and a complete network, and compare the spread of the error of the
consensus.

One catch: a star is periodic, so its opinions would swing between the
centre and the leaves for ever (remember the rings above?). So let every
node also listen a little to itself, with
[`to_looped()`](https://stocnet.github.io/manynet/reference/modif_loops.html).
That is realistic anyway: few of us drop our own estimate entirely.

``` r

set.seed(1)
estimates <- 50 + rnorm(32, 0, 10)
consensus <- play_opinion(ison_networkers, estimates)
tail(as_opinion(consensus), 1)
mean(estimates)
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

### When does talking help?

In the DeGroot model, talking cannot make the crowd wiser *on average*:
the consensus weights the estimates unequally, and the plain mean, with
equal weights, has the smallest expected error. But in experiments,
Becker, Brackbill and Centola (2017) found that talking made crowds
wiser, if their network was egalitarian: if no node had much more
influence than the others. One reason is that people whose first
estimate is near the truth tend to revise it less. So the accurate
people are more stubborn.

Let the stubbornness of each node be 1 minus its error, as a share of
the largest error. Then the most accurate node is a zealot, and the
least accurate is not stubborn at all.
[`node_by_opinion()`](https://stocnet.github.io/netrics/reference/measure_opinion.html)
gives the opinion of each node at the end, from the nodal attributes
“opinions” and “stubbornness”. Compare the error of the mean before and
after the nodes talk, 200 times, in a random network, a star, and the
networkers, each with
[`to_looped()`](https://stocnet.github.io/manynet/reference/modif_loops.html)
as before.

``` r

set.seed(1)
estimates <- 50 + rnorm(32, 0, 10)
error <- abs(estimates - 50)
crowd <- to_looped(generate_random(32, 0.2)) |>
  mutate_nodes(opinions = estimates, stubbornness = 1 - error / max(error))
ends <- node_by_opinion(crowd)
c(before = abs(mean(estimates) - 50), after = abs(mean(ends) - 50))
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

## Misinformation

What happens when some nodes spread a false opinion and never change it?
(Every group chat seems to have one.) Such a fully stubborn node is
sometimes called a zealot . In the DeGroot model, one zealot in a
strongly connected network pulls every other node to its opinion in the
end. Where the others are also a little stubborn, the pull is partial,
and it depends on where the zealot sits.

Spreading a false claim and believing it are two different processes.
Passing a story on is a diffusion: each node has heard the story or not,
as in the “Diffusion and Contagion” tutorial. Believing it is learning:
how much a node believes the story moves towards what the nodes it
listens to believe. A story can reach everyone and convince no one, or
reach only a few and still shift what the whole network believes. This
section is about belief.

### Where does misinformation do most harm?

Let the truth be 0, and let the networkers start near it. Then let one
node be a zealot at 1, and all others hold on to their first opinion
with a stubbornness of 0.3. Compare a zealot with the highest influence
and one with the lowest, by the mean opinion at the end.

``` r

set.seed(2)
first <- rnorm(32, 0, 0.1)
influence <- node_by_influence(ison_networkers)
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

What could stop a zealot? Try a second zealot at the truth, 0, as a
fact-checker, and move it to different places in the network.

### Moving the window

A zealot whose opinion is far from everyone else’s has no pull where
people listen only to opinions near their own. Joseph Overton described
a “window” of the policies that the public will accept at a given time.
A policy outside the window is unthinkable, whoever argues for it. But
the window can move. In a model of bounded confidence, each node’s
`confidence` is such a window, centred on its own opinion.

Let the networkers start between 0 and 0.4, and let the node with the
most influence be a zealot. Play the Hegselmann–Krause rule with a
`confidence` of 0.2. First, let the zealot jump straight to 1. Then let
the zealot move in small steps of 0.1, from 0.3 to 1, and let each step
start from the opinions at the end of the step before.

``` r

set.seed(1)
first <- runif(32, 0, 0.4)
zealot <- which.max(node_by_influence(ison_networkers))
stubborn <- ifelse(seq_len(32) == zealot, 1, 0)
```

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

So an opinion that no one will hear today can become acceptable
tomorrow, if the opinions in between are heard first. Try a wider or a
narrower `confidence`, or larger steps. How large can a step be before
the window stops following?

## Equilibria and disequilibria

Each model in this tutorial settles in its own way, or does not settle
at all:

| Outcome | What gives it | Where we saw it |
|----|----|----|
| Consensus | A strongly connected, aperiodic network, with no stubbornness | DeGroot on `ison_networkers` |
| Lasting disagreement | Stubbornness, which pulls each node back to its first opinion | The path A–B–C |
| Fragmentation | Bounded confidence, which cuts the ties between distant opinions | Hegselmann–Krause and Deffuant |
| Polarisation | Repulsion on a balanced signed network | `irps_wwi` in 1914 |
| Capture | One zealot in a network with no other stubborn node | Misinformation |
| No equilibrium | A periodic network, where opinions can swing for ever | A ring with an even number of nodes |

The last is a disequilibrium . On a ring of four nodes, each node
averages its two neighbours. Start with opinions of 1, 0, 1, and 0, and
see what happens at each step. Then try a ring of five, with 1, 0, 1, 0,
and 0.

**Try it yourself**: This section includes an interactive quiz in the
live tutorial — run `run_tute()` at the R console to try it.

A real group may still settle on such a network, because people also
listen to themselves. A self-weight, a self-loop in the network, breaks
the swing. Try `play_opinion(to_looped(create_ring(4)), c(1, 0, 1, 0))`,
and see
[`to_influence()`](https://stocnet.github.io/manynet/reference/modif_influence.html)
for how the self-weight is set.

## Glossary

Where opinions end up depends on whom each node listens to, and how
much. In the DeGroot model, a network that is strongly connected and
aperiodic always reaches a consensus, and how fast it gets there depends
on its structure. Stubbornness keeps a disagreement alive, because each
node is pulled back towards its first opinion. Bounded confidence breaks
a group into clusters of opinion, since nodes stop listening to opinions
too far from their own, so diverse groups learn from one another only
where their members listen widely. Repulsion on a balanced signed
network polarises opinions into two camps. The nodes with the most
influence on the end are close to, but not the same as, those with the
highest eigenvector centrality. Talking can make a crowd wiser, but only
where no node has too much influence, and a zealot does most harm where
it has most influence. A zealot too far from the others is not heard,
but one that moves in small steps can move the whole window of
acceptable opinions. Finally, not every network settles: on a periodic
network, opinions can swing for ever.

Here are some of the terms that we have covered in this module:

- Aperiodic : An aperiodic network is a network where the greatest
  common divisor of the lengths of its cycles is one.
- Balance : A signed network is structurally balanced where its nodes
  split into two camps, with positive ties inside each camp and negative
  ties between them.
- Bounded Confidence : In a bounded confidence model, a node listens
  only to those whose opinions are within some distance of its own.
- Connected : A connected network is one with a single (strong)
  component.
- Periodic : A periodic network is one where the lengths of all its
  cycles share a divisor greater than one, so that a process on it can
  swing for ever.
- Stubbornness : The stubbornness of a node is how much it holds on to
  its first opinion, from 0 for none to 1 for a node that never moves.
- Wisdom Of Crowds : The wisdom of crowds is that the mean of many
  independent estimates is often closer to the truth than most of the
  estimates.
- Zealot : A zealot is a fully stubborn node, with a stubbornness of 1,
  that never changes its opinion, however the others try to persuade it.

### Next: Modelling with ERGMs

In this tutorial and the last, the network was given, and we asked what
spreads or settles on it. But ties also form for reasons, for example
because people choose others with opinions like their own. The
“Modelling with ERGMs” tutorial models the ties themselves, with
exponential random graph models.
