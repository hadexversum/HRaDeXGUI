# Estimated k plot

## What is this showing?

After fitting process, we have an exponential curve describing each uptake curve on peptide level. Assuming that each $n_i$ value is a percentage of the exchange group $i$ in the peptide from the range (0, 1) then probability of achieving exchange speed $k_i$ is $n_i/n$.

So, the estimated k values are calculated as follows:

$$ k_{est} = \sum_0^3 \frac{n_i}{n} \cdot k_i $$

For each residue, the $k_est$ value from the shortest peptide overlapping said reside is selected.

On X axis there are positions of each residue.
On Y axis there are estimated $k$ values, as explained above.

Lack of value for a resideu indicates either that this region was not covered or that the fitting results were not sufficient.

## How to interpret this results?

The tendencies of exchange process are visible for the regions. 

## Additionally

This form of presentation flattens the number of parameters and is readable for people with color blindeness. 