#!/usr/bin/env nextflow

// Usage: nextflow run main.nf --reads "data/*_R{1,2}.fq.gz" -profile slurm,singularity --mode "fastqc"
// INIT: nextflow run main.nf -profile singularity,init
// TEST: nextflow run main.nf -profile singularity,test -resume
// QC: nextflow run main.nf -profile singularity,slurm --reads "data/*_R{1,2}.fq.gz" --mode "fastqc"
// CLEAN READS: nextflow run main.nf -profile singularity,slurm --reads "data/*_R{1,2}.fq.gz" --mode "clean"
// ALL: nextflow run main.nf -profile singularity,slurm --reads "data/*_R{1,2}.fq.gz" --mode "all"

nextflow.enable.dsl=2

include { CMAPSCORE_KS; CMAPSCORE_XSUM; CMAPSCORE_G0; CMAPSCORE_G1; CMAPSCORE_G2; CMAPSCORE_ZH } from "./module/cmap"

workflow {
    CMAPSCORE_KS()
    CMAPSCORE_G0()
    CMAPSCORE_G1()
    CMAPSCORE_G2()
    CMAPSCORE_ZH()
    CMAPSCORE_XSUM()
}


workflow.onComplete {
    log.info ( workflow.success ? "\nDone!" : "Oops .. something went wrong" )
}
