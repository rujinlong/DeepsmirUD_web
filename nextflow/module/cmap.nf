process CMAPSCORE_KS {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"


    output:
    path("*.rds")

    """
    run_cmap.R $params.profilefile KSScore $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_XSUM {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    output:
    path("*.rds")

    """
    run_cmap.R $params.profilefile XSumScore $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_G0 {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"


    output:
    path("*.rds")

    """
    run_cmap.R $params.profilefile GSEAweight0Score $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_G1 {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"


    output:
    path("*.rds")

    """
    run_cmap.R $params.profilefile GSEAweight1Score $params.npermutation $task.cpus $params.topn 0
    """
}

process CMAPSCORE_G2 {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"


    output:
    path("*.rds")

    """
    run_cmap.R $params.profilefile GSEAweight2Score $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_ZH {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    output:
    path("*.rds")

    """
    run_cmap.R $params.profilefile ZhangScore $params.npermutation $task.cpus $params.topn 0
    """
}
