# Define path to test-remind
testRemind <- "/home/johannes/tmp_testRemind"

# Define remind_repo option
opts <- options()$remind_repos
options(remind_repos = list(
  "scp://cluster.pik-potsdam.de/p/projects/rd3mod/inputdata/output" = list(
    username = "jokoch",
    ssh_private_keyfile = "/home/johannes/.ssh/id_rsa",
    keypasswd = "jo_Ha!N1"),
  "scp://cluster.pik-potsdam.de/p/projects/remind/inputdata/CESparametersAndGDX" = list(
    username = "jokoch",
    ssh_private_keyfile = "/home/johannes/.ssh/id_rsa",
    keypasswd = "jo_Ha!N1")
))
withr::defer(options(remind_repos = opts), teardown_env())
