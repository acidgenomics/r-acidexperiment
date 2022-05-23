data(syntactic, package = "AcidTest", envir = environment())
funs <- list(
    "camelCase" = camelCase,
    "dottedCase" = dottedCase,
    "snakeCase" = snakeCase,
    "upperCamelCase" = upperCamelCase
)

test_that("Unnamed", {
    for (fun in funs) {
        object <- 1L
        expect_identical(fun(object), object)
    }
})

test_that("Named", {
    Map(
        fun = funs,
        expected = c(
            "helloWorld",
            "hello.world",
            "hello_world",
            "HelloWorld"
        ),
        f = function(fun, expected) {
            object <- c("hello.world" = 1L)
            expect_identical(names(fun(object)), expected)
        }
    )
})

test_that("factor", {
    Map(
        fun = funs,
        levels = list(
            "camelCase" = c("group1", "group2"),
            "dottedCase" = c("group.1", "group.2"),
            "snakeCase" = c("group_1", "group_2"),
            "upperCamelCase" = c("Group1", "Group2")
        ),
        names = list(
            "camelCase" = c("sample1", "sample2", "sample3", "sample4"),
            "dottedCase" = c("sample.1", "sample.2", "sample.3", "sample.4"),
            "snakeCase" = c("sample_1", "sample_2", "sample_3", "sample_4"),
            "upperCamelCase" = c("Sample1", "Sample2", "Sample3", "Sample4")
        ),
        f = function(fun, levels, names) {
            object <- syntactic[["factor"]]
            x <- fun(object, names = TRUE)
            expect_identical(
                object = levels(x),
                expected = levels
            )
            expect_identical(
                object = names(x),
                expected = names
            )
            x <- fun(object, names = FALSE)
            expect_identical(
                object = names(x),
                expected = names(object)
            )
        }
    )
})

test_that("list", {
    Map(
        fun = funs,
        expected = list(
            "camelCase" = c("itemA", "itemB"),
            "dottedCase" = c("item.a", "item.b"),
            "snakeCase" = c("item_a", "item_b"),
            "upperCamelCase" = c("ItemA", "ItemB")
        ),
        f = function(fun, expected) {
            object <- syntactic[["list"]]
            expect_identical(
                object = names(fun(object)),
                expected = expected
            )
        }
    )
})

test_that("matrix", {
    Map(
        fun = funs,
        expected = list(
            "camelCase" = list(
                c("alabama", "alaska", "arizona"),
                c("murder", "assault", "urbanPop")
            ),
            "dottedCase" = list(
                c("alabama", "alaska", "arizona"),
                c("murder", "assault", "urban.pop")
            ),
            "snakeCase" = list(
                c("alabama", "alaska", "arizona"),
                c("murder", "assault", "urban_pop")
            ),
            "upperCamelCase" = list(
                c("Alabama", "Alaska", "Arizona"),
                c("Murder", "Assault", "UrbanPop")
            )
        ),
        f = function(fun, expected) {
            object <- syntactic[["matrix"]][seq_len(3L), seq_len(3L)]
            expect_identical(
                object = dimnames(fun(
                    object = object,
                    rownames = TRUE,
                    colnames = TRUE
                )),
                expected = expected
            )
        }
    )
})

mcols(df) <- DataFrame(TEST = seq_len(ncol(df)))
metadata(df) <- list(TEST = "XXX")

test_that("DataFrame", {
    for (fun in funs) {
        x <- fun(
            object = df,
            rownames = TRUE,
            colnames = TRUE,
            mcols = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "DataFrame")
    }
})

test_that("GenomicRanges", {
    Map(
        fun = funs,
        expected = list(
            "camelCase" = c("geneId", "geneName"),
            "dottedCase" = c("gene.id", "gene.name"),
            "snakeCase" = c("gene_id", "gene_name"),
            "upperCamelCase" = c("GeneId", "GeneName")
        ),
        f = function(fun, expected) {
            x <- fun(
                object = gr,
                names = TRUE,
                mcols = TRUE
            )
            expect_s4_class(x, "GenomicRanges")
            expect_identical(
                object = colnames(mcols(x)),
                expected = expected
            )
        }
    )
})

test_that("SummarizedExperiment", {
    for (fun in funs) {
        x <- fun(
            object = rse,
            rownames = TRUE,
            colnames = TRUE,
            assayNames = TRUE,
            rowData = TRUE,
            colData = TRUE,
            metadata = TRUE
        )
        expect_s4_class(x, "SummarizedExperiment")
    }
})
