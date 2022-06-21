process CMAPSCORE_KS {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    input:
    path(profile)

    output:
    path("*.rds")

    """
    run_cmap.R $profile KSScore $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_XSUM {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    input:
    path(profilefile)

    output:
    path("*.rds")

    """
    run_cmap.R $profilefile XSumScore $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_G0 {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    input:
    path(profilefile)

    output:
    path("*.rds")

    """
    run_cmap.R $profilefile GSEAweight0Score $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_G1 {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    input:
    path(profilefile)

    output:
    path("*.rds")

    """
    run_cmap.R $profilefile GSEAweight1Score $params.npermutation $task.cpus $params.topn 0
    """
}

process CMAPSCORE_G2 {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    input:
    path(profilefile)

    output:
    path("*.rds")

    """
    run_cmap.R $profilefile GSEAweight2Score $params.npermutation $task.cpus $params.topn 0
    """
}


process CMAPSCORE_ZH {
    label "container"
    errorStrategy 'finish'
    publishDir "$params.outdir/$params.runid/"

    input:
    path(profilefile)

    output:
    path("*.rds")

    """
    run_cmap.R $profilefile ZhangScore $params.npermutation $task.cpus $params.topn 0
    """
}