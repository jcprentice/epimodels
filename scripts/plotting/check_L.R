dataset = "fb-fes1"; scens = 1:10
# dataset = "fb-fes2"; scens = 1:11

plts <- map(scens, \(i) {
    plt <- check_L(dataset, i)
    ggsave(str_glue("datasets/{dataset}/gfx/L-scen{i}.png"),
           plot = plt, width = 12, height = 6)
    plt
})
