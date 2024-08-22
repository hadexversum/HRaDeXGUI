## Aggregation method

There are two ways to aggregate the information from peptide level into high-resolution:

### Shortest

The `shortest` method assigns the HDX parameters of the shortest peptide that covers this residue to a given residue. If multiple peptides of equal length overlap a single residue, we prioritize the peptide closest to the N-terminus of a protein.


### Weighted approach

The `weighted average` approach uses residue averaging to assess the HDX of a single residue, as [described](https://pubs.acs.org/doi/10.1007/s13361-014-1033-6). This method computes the weighted average of all exchange groups assigned to the peptides. The weights are inversely proportional to the theoretical maximum uptake. 


For more information check the [documentation](https://hadexversum.github.io/HRaDeX/).