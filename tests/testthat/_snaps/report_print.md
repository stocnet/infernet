# each netreg variant prints its title, its estimates, and its footer

    Code
      print(estimate_mrqap(weight ~ ego(Age) + same(Grp), g, times = 20, control = list(
        seed = 1)))
    Output
      # Linear MRQAP of weight
      
      # A tibble: 3 x 5
        term        estimate statistic p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <noquote>
      1 (Intercept)  -1.10      -4.40  <NA>             
      2 ego_Age       0.0502     7.74  < 0.05  *        
      3 same_Grp     -0.0917    -0.768 0.25             
      
      # Null: 20 permutations of each predictor, by double semi-partialling.
      # Controls: seed = 1
      # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1

---

    Code
      print(estimate_mrqap(. ~ ego(Age) + alter(Age), b, times = 20, control = list(
        seed = 1)))
    Output
      # Logistic MRQAP of ties
      
      # A tibble: 3 x 5
        term        estimate statistic p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <noquote>
      1 (Intercept)   0.467      0.329 <NA>             
      2 ego_Age       0.0543     2.31  0.05    *        
      3 alter_Age    -0.0693    -2.83  < 0.05  *        
      
      # Null: 20 permutations of each predictor, by double semi-partialling.
      # Controls: seed = 1
      # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1

---

    Code
      print(estimate_mrqap(weight ~ diff(Age) | sender, g, times = 20, control = list(
        seed = 1)))
    Output
      # Linear MRQAP of weight, with sender fixed effects
      
      # A tibble: 1 x 5
        term     estimate statistic p.value signif   
        <chr>       <dbl>     <dbl> <chr>   <noquote>
      1 diff_Age  0.00491     0.582 0.7              
      
      # Null: 20 permutations of the outcome.
      # Controls: seed = 1

---

    Code
      print(suppressMessages(estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
      times = 5, control = list(seed = 1))))
    Output
      # Logistic MRQAP of advice
      
      # A tibble: 3 x 5
        term        estimate statistic p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <noquote>
      1 (Intercept)  -0.269     -0.928 <NA>             
      2 ego_age      -0.0102    -1.53  0.4              
      3 friends       2.14       7.41  < 0.2            
      
      # Null: 5 permutations of each predictor, by double semi-partialling.
      # Permutations relabel senders, receivers, and reporters alike.
      # Controls: seed = 1

---

    Code
      print(suppressMessages(estimate_mrqap(. ~ inertia + ego(age), qap_net_panel(),
      times = 5, control = list(seed = 1))))
    Output
      # Logistic MRQAP of ties
      
      # A tibble: 3 x 5
        term        estimate statistic p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <noquote>
      1 (Intercept)  -0.846      -1.41 <NA>             
      2 inertia       3.03        9.56 < 0.2            
      3 ego_age      -0.0158     -1.07 0.2              
      
      # Panel: 2 waves, each fitted against the wave before.
      # Null: 5 permutations of each predictor, by double semi-partialling.
      # Permutations relabel the nodes of every wave alike.
      # Controls: seed = 1

# a mixed model prints its random intercepts in its summary

    Code
      print(fit)
    Output
      # Linear mixed MRQAP of weight, with sender random intercepts
      
      # A tibble: 2 x 5
        term        estimate statistic p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <noquote>
      1 (Intercept)  -1.12       -3.63 < 0.2            
      2 ego_Age       0.0500      6.24 < 0.2            
      
      # Null: 5 permutations of the outcome.
      # Controls: seed = 1

---

    Code
      print(summary(fit))
    Output
      # Linear mixed MRQAP of weight, with sender random intercepts
      
      # A tibble: 2 x 7
        term        estimate statistic p.lower p.upper p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <chr>   <chr>   <noquote>
      1 (Intercept)  -1.12       -3.63 < 0.2   1       < 0.2            
      2 ego_Age       0.0500      6.24 1       < 0.2   < 0.2            
      
      # Random intercepts
      # A tibble: 1 x 3
        group  variance    sd
        <chr>     <dbl> <dbl>
      1 sender   0.0162 0.127
      # A tibble: 10 x 3
         group  level intercept
         <chr>  <chr>     <dbl>
       1 sender N01   -0.0916  
       2 sender N02    0.000152
       3 sender N03    0.0704  
       4 sender N04    0.0652  
       5 sender N05   -0.0383  
       6 sender N06    0.106   
       7 sender N07   -0.00228 
       8 sender N08    0.0478  
       9 sender N09   -0.127   
      10 sender N10   -0.0309  
      
      # Fit of the observed model
       r.squared adj.r.squared sigma logLik AIC BIC df.residual nobs permute times
              NA            NA 0.483  -69.9 148 158          86   90 outcome     5
      
      # Controls: seed = 1

# a summary prints the full table, the fit, and the dropped terms

    Code
      print(summary(fit))
    Output
      # Linear MRQAP of weight
      
      # A tibble: 3 x 7
        term        estimate statistic p.lower p.upper p.value signif   
        <chr>          <dbl>     <dbl> <chr>   <chr>   <chr>   <noquote>
      1 (Intercept)  -1.10      -4.40  <NA>    <NA>    <NA>             
      2 ego_Age       0.0502     7.74  1       < 0.05  < 0.05  *        
      3 same_Grp     -0.0917    -0.768 0.1     0.9     0.25             
      
      # Fit of the observed model
       r.squared adj.r.squared sigma logLik AIC BIC df.residual nobs   permute times
           0.408         0.395 0.501  -63.9 136 146          87   90 predictor    20
      
      # Terms that the formula changed
      # A tibble: 1 x 4
        term    status as    reason                                                   
        <chr>   <chr>  <chr> <chr>                                                    
      1 inertia drops  <NA>  needs a longitudinal network, where it reads the outcome~
      
      # Controls: seed = 1
      # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1

# each network test prints what it tested and its null

    Code
      print(test_random(net, netrics::net_by_density, times = 20, control = list(
        seed = 1)))
    Output
      # CUG test of netrics::net_by_density()
      
      # A tibble: 1 x 5
        statistic null.mean null.sd p.lower p.upper
            <dbl>     <dbl>   <dbl> <chr>   <chr>  
      1     0.357     0.357       0 1       1      
      
      # Null: 20 random networks with the same size and density.
      # Controls: seed = 1

---

    Code
      print(test_configuration(net, netrics::net_by_transitivity, times = 20,
      control = list(seed = 1)))
    Output
      # Configuration test of netrics::net_by_transitivity()
      
      # A tibble: 1 x 5
        statistic null.mean null.sd p.lower p.upper
            <dbl>     <dbl>   <dbl> <chr>   <chr>  
      1      0.45     0.353   0.131 0.9     0.4    
      
      # Null: 20 random networks with the same degrees.
      # Controls: seed = 1

---

    Code
      print(test_permutation(net, netrics::net_by_density, times = 20, control = list(
        seed = 1)))
    Output
      # QAP test of netrics::net_by_density()
      
      # A tibble: 1 x 5
        statistic null.mean null.sd p.lower p.upper
            <dbl>     <dbl>   <dbl> <chr>   <chr>  
      1     0.357     0.357       0 1       1      
      
      # Null: 20 permutations of the node labels.
      # Controls: seed = 1

---

    Code
      print(test_parameter(fit, "alter_Age", times = 20, control = list(seed = 1)))
    Output
      # Parameter test of alter_Age = 0 in a logistic MRQAP of ties
      
      # A tibble: 1 x 6
        statistic null.mean null.sd p.lower p.upper signif   
            <dbl>     <dbl>   <dbl> <chr>   <chr>   <noquote>
      1      9.13      1.25    1.78 1       0.0476  *        
      
      # Null: 20 permutations of the tested predictors, by double semi-partialling.
      # The p-value is p.upper: only a larger statistic counts against the null.
      # Controls: seed = 1
      # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1

# a diffusion model and its tests print

    Code
      print(d)
    Output
      # 5 simulated SI diffusions
      
      # A tibble: 38 x 6
           sim  time     n     S     s     I
         <int> <dbl> <dbl> <dbl> <dbl> <int>
       1     1     0     8     7     1     1
       2     1     1     8     7     1     1
       3     1     2     8     6     3     2
       4     1     3     8     4     3     4
       5     1     4     8     3     2     5
       6     1     5     8     2     1     6
       7     2     0     8     7     1     1
       8     2     1     8     7     1     1
       9     2     2     8     6     3     2
      10     2     3     8     6     3     2
      # i 28 more rows
      
      # Steps until every node adopted: median 9, from 7 to 10.
      # Controls: seed = 1

---

    Code
      print(test_distributions(f, d, times = 20, control = list(seed = 1)))
    Output
      # Two-sample test of two sets of simulated diffusions
      
      # A tibble: 1 x 6
        statistic null.mean null.sd p.lower p.upper signif   
            <dbl>     <dbl>   <dbl> <chr>   <chr>   <noquote>
      1      37.9      63.0    54.1 0.476   0.571            
      
      # Null: 20 permutations of the pooled diffusions.
      # The p-value is p.upper: only a larger statistic counts against the null.
      # Controls: seed = 1

# a specification and a control print

    Code
      print(specify_mrqap(advice ~ edges + ego(age) + same(dept)))
    Output
      # An MRQAP formula read from a formula for ergm
      
      advice ~ ego(age) + same(dept)
      
      # adapted edges: is the intercept that the model always has

---

    Code
      print(suppressWarnings(specify_ergm(advice ~ ego(age) + same(dept) + recip(
        friends))))
    Output
      # An ergm formula read from a formula for an MRQAP
      
      advice ~ edges + nodeocov("age") + nodematch("dept")
      
      # adapted ego(age) -> nodeocov("age")
      # adapted same(dept) -> nodematch("dept")
      # drops recip(friends): needs .data to build its matrix for edgecov()

---

    Code
      print(control_mrqap(seed = 1, family = "binomial"))
    Output
      # Controls for an MRQAP: 2 set, 13 at their defaults
      
        family  "binomial"
        seed    1

