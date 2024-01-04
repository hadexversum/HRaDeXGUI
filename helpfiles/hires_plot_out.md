# High-resolution plot

## What is this showing?

High resolution plot presents the high-resolution results from the fitting process for each residue separately. The results from the fitting process are presented using color code:

$$ color = rgb(\frac{n_1}{n}, \frac{n_2}{n}, \frac{n_3}{n}) $$

where $n = n_1 + n_2 + n_3 $. Values are scaled to ensure them being from the correct range, even if the fitting process is performed on values in daltons.

Color code based on the fitted uptake curve is assesed for each peptide. Then, for each residue the information from the shortest peptide overlaping this residue is selected.

For non covered regions of the protein sequence the grey color is presented with NA values.

## How to interpret this results?

To interpret the color code, the color legend is provided on the right of the main panel, next to the plots. Generally speaking, the pure red regions (usually C- or N-terminus) indicated immediete exchange. Red-ish regions indicate mostly fast regions, green-ish medium and blue-ish mostly slow exchange. The black colored regions present no exchange observed under the experiment course.