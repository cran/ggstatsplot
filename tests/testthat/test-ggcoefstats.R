# errors ------------------------------------------

test_that("ggcoefstats doesn't work if no estimate column found", {
  expect_snapshot_error(ggcoefstats(iris))
})

# default plots for each statistic ------------------------------------------

test_that("default plots are rendered correctly for each type of statistic", {
  skip_if_not_installed("withr")

  set.seed(123)
  expect_doppelganger(
    title = "t-statistic",
    fig = ggcoefstats(stats::lm(formula = wt ~ am * cyl, data = mtcars))
  )

  set.seed(123)
  expect_doppelganger(
    title = "F-statistic",
    fig = ggcoefstats(aov(yield ~ N * K, npk))
  )

  set.seed(123)
  expect_doppelganger(
    title = "F-statistic with omega",
    fig = ggcoefstats(stats::aov(wt ~ mpg * am, mtcars), effectsize.type = "omega")
  )

  df <- as.data.frame(Titanic)

  mod_glm <- stats::glm(
    formula = Survived ~ Sex + Age,
    data = df,
    weights = df$Freq,
    family = stats::binomial(link = "logit")
  )

  set.seed(123)
  expect_doppelganger(
    title = "z-statistic",
    fig = ggcoefstats(mod_glm, conf.level = 0.90)
  )

  skip_if_not_installed("survival")
  withr::local_package("survival")

  set.seed(123)
  expect_doppelganger(
    title = "chi2-statistic",
    fig = suppressWarnings(ggcoefstats(
      survival::coxph(Surv(time, status) ~ age + sex + frailty(inst), lung)
    ))
  )
})

df_meta <- tibble(
  estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
  std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
)

test_that("meta-analysis works", {
  skip_on_cran()
  skip_if_not_installed("metafor")
  skip_on_os(c("windows", "linux", "solaris"))

  set.seed(123)
  p_meta <- suppressWarnings(ggcoefstats(
    df_meta,
    meta.analytic.effect = TRUE,
    bf.message = TRUE
  ))

  expect_s3_class(p_meta, "ggplot")

  skip_on_os("mac", c("i386", "x86_64"))
  set.seed(123)
  expect_doppelganger(
    title = "meta-analysis works",
    fig = p_meta
  )
})

# plot modifications--------------------------------------------------

test_that(
  "plot modifications work as expected",
  {
    set.seed(123)
    mod1 <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    set.seed(123)
    expect_doppelganger(
      title = "plot modifications",
      fig = suppressWarnings(ggcoefstats(
        x = mod1,
        conf.level = 0.99,
        exclude.intercept = TRUE,
        only.significant = TRUE,
        package = "ggsci",
        palette = "category20c_d3",
        digits = 3L
      ))
    )

    set.seed(123)
    mod2 <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ vore * brainwt,
      na.action = na.omit
    )

    set.seed(123)
    expect_doppelganger(
      title = "sorting works",
      fig = ggcoefstats(
        x = mod2,
        exclude.intercept = FALSE,
        sort = "ascending",
        effectsize.type = "omega",
        title = "mammalian sleep",
        subtitle = "Source: `{ggplot2}` package",
        package = "wesanderson",
        palette = "BottleRocket2",
        digits = 3L
      )
    )
  }
)

# edge cases -------------------------------------

test_that(
  "works when CIs unavailable",
  {
    set.seed(123)
    df_base <- tidy_model_parameters(stats::lm(wt ~ am * cyl, mtcars))

    set.seed(123)
    expect_doppelganger(
      title = "CIs missing",
      fig = ggcoefstats(dplyr::select(df_base, -dplyr::matches("conf")), statistic = "t")
    )

    expect_snapshot_error(
      ggcoefstats(
        dplyr::bind_rows(df_base, df_base),
        statistic = "t"
      )
    )
  }
)

# meta subtitle and caption -------------------------------------

test_that(
  "meta analysis subtitle and caption",
  {
    skip_on_cran()
    skip_if_not_installed("metafor")
    skip_if_not_installed("metaBMA")
    skip_if_not_installed("metaplus")

    set.seed(123)
    subtitle_expr <- suppressWarnings(meta_analysis(df_meta, type = "p"))

    set.seed(123)
    caption_expr <- suppressWarnings(meta_analysis(df_meta, type = "bayes"))

    set.seed(123)
    ggcoef_subtitle <- extract_subtitle(suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = FALSE,
      meta.type = "p"
    )))

    set.seed(123)
    ggcoef_caption <- extract_caption(suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = TRUE,
      meta.type = "p"
    )))

    expect_identical(subtitle_expr$expression[[1L]], ggcoef_subtitle)
    expect_identical(caption_expr$expression[[1L]], ggcoef_caption)
  }
)
