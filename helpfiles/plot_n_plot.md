# Population 

## What is this showing?

This plot shows cumulative n value for each peptide based on the fitted function. As described before, the fitted function is in a form of:

$$ D(t) = \sum^i n_i \cdot ( 1 - exp^( { -k_i \cdot t }) )  $$

with $i\in{1, 2, 3}$

For the normalized uptake curve (that tends to 1), desired situation is:

$$  \sum n_i = 1 $$

For not-normalized uptake curves the situation is slightly different, as we want the $n$ value to be smaller than the highest possible value - maximal deuterium uptake based on the peptide sequence (MaxUptake). Moreover, the situation when $n$ is nowhere close to MaxUptake is not strange, as it accounts for the back-exchange process.

## How to interpret this results?

This plot serves as quality control to check if the results of the fitting process are valid. 
For normalized curves, all $n$ values should be close to 1.
For un-normalized curves, the $n$ values should be small than the MaxUptake value for the peptide.

## Additionaly

This plot serves as quality control for the fitting process. If there is anything suspicious, try different fitting parameters and check if the situation improves.