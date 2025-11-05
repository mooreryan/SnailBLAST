# crawl creates correct BLAST commands with outfmt specifiers and extra blast arguments

    Code
      cat(paste(full_commands, collapse = "\n"))
    Output
      /usr/bin/blastn -db test_data/real_db_1 -query test_data/real_queries_1.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5
      /usr/bin/blastn -db test_data/real_db_1 -query test_data/real_queries_2.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5
      /usr/bin/blastn -db test_data/real_db_2 -query test_data/real_queries_1.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5
      /usr/bin/blastn -db test_data/real_db_2 -query test_data/real_queries_2.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5

# crawl creates correct BLAST commands with outfmt specifiers, extra blast arguments, and long names

    Code
      cat(paste(full_commands, collapse = "\n"))
    Output
      /usr/bin/blastn -db test_data/real_db_1 -query test_data/real_queries_1.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5
      /usr/bin/blastn -db test_data/real_db_1 -query test_data/real_queries_2.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5
      /usr/bin/blastn -db test_data/real_db_2 -query test_data/real_queries_1.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5
      /usr/bin/blastn -db test_data/real_db_2 -query test_data/real_queries_2.fasta -out [REDACTED] -outfmt 6 qaccver saccver pident length -evalue 1e-10 -max_target_seqs 5

