# Test Data

## Real Data

### DBs

- `real_db_1`: the full _E. coli_ RNR sequence
- `real_db_2`: contains only the last bit of the RNR sequence

Creating the BLAST DBs

```
makeblastdb -in real_db_1.fasta -out real_db_1 -dbtype nucl
makeblastdb -in real_db_2.fasta -out real_db_2 -dbtype nucl
```

### Queries

- `real_queries_1`: contains 4 "sequences" from the beginning and end of the RNR sequence (this will have hits to both DBs)
- `real_queries_2`: contains 2 "sequences" from the beginning (this should only have hits to the first DB)
- `real_queries_3`: contains the _exact same sequences_ with alterned names as `real_queries_2`. It is here to ensure there aren't any bugs when one of the number of query files or number of target DBs isn't a factor of the other.

### Expected Hits

Create a file to hold all these hits

```
blastn -query real_queries_1.fasta -db real_db_1 -outfmt 6 > expected_real_hits.tsv
blastn -query real_queries_1.fasta -db real_db_2 -outfmt 6 >> expected_real_hits.tsv
blastn -query real_queries_2.fasta -db real_db_1 -outfmt 6 >> expected_real_hits.tsv
blastn -query real_queries_2.fasta -db real_db_2 -outfmt 6 >> expected_real_hits.tsv
```

## Very Small Test DB

In the "tiny" directory.

Make a BLAST DB from the nrdA sequence:

```
makeblastdb -in nrdA.fna -out nrdA -dbtype nucl
```

Run the BLAST to get the expected results:

```
blastn -query good_queries.fna -db nrdA -outfmt 6 -out good_queries_result.tsv
```
