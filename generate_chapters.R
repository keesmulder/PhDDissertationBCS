library(stringr)
library(tidyverse)

out_folder <- "C:/Dropbox/Research/PhDDissertationBCS/chapters/"
glm_path <- "C:/Dropbox/Research/BayesMultCircCovariates/Article JMP/"


inside_curly <- Vectorize(function(string) {
  stringr::str_extract(string, "\\{.*?\\}(?!.*\\})")
})

# Read a phd chapter and remove junk.
read_and_refactor_chapter <- function(input_path) {
  text <- readLines(input_path)

  # Remove commented out lines.
  strip_text <- text[!grepl("^%", text)]

  # Information to extract as metadata.
  pkg_lines  <- grep("\\usepackage", strip_text, value = TRUE)
  pkg_lines  <- pkg_lines[!grepl("^\\\\documentclass", pkg_lines)]
  new_cmds   <- grep("newcommand", strip_text, value = TRUE)
  title      <- inside_curly(grep("\\\\title", strip_text, value = TRUE))
  authors    <- inside_curly(grep("\\\\author", strip_text, value = TRUE))
  sections   <- inside_curly(grep("\\\\section", strip_text, value = TRUE))

  # Indices to remove.
  first_sec  <- grep("\\\\section", strip_text, value = FALSE)[1]
  bib_idx    <- grep("\\\\bibliography", strip_text, value = FALSE)
  end_idx    <- grep("\\\\end\\{document\\}", strip_text, value = FALSE)
  header_idx <- 1:(first_sec - 1)

  body_text <- strip_text[-c(bib_idx, end_idx, header_idx)]

  out <- list(title            = title,
              authors          = authors,
              usepackage_lines = pkg_lines,
              new_commands     = new_cmds,
              sections         = sections,
              text             = body_text)
  class(out) <- c("tex_chapter", "list")
  out
}


read_and_save_multiple_chapters <- function(filepaths,
                                            outloc = "C:/Dropbox/Research/PhDDissertationBCS/",
                                            remove_preamble_lines = character(0)) {

  outloc_ch <- paste0(outloc, "chapters/")
  outloc_ch

  # Standard naming.
  if (is.null(names(filepaths))) {
    nms <- paste0("chapter_", seq_along(filepaths))
  } else {
    nms <- names(filepaths)
  }

  preamble_lines <- character(0)

  # Refactor and save.
  for (filepath_nm in nms) {
    filepath <- filepaths[filepath_nm]
    chapter <- read_and_refactor_chapter(filepath)

    cat("Read chapter '", chapter$title, "'. Processing...\n---\n", sep = "")

    preamble_lines <- c(preamble_lines,
                        chapter$usepackage_lines,
                        chapter$new_commands)


    saveRDS(chapter, file = paste0(outloc_ch, filepath_nm, ".rds"))

    filecon <- file(paste0(outloc_ch, filepath_nm, ".tex"))
    writeLines(chapter$text, con = filecon)
    close(filecon)

  }


  # COMMAND LINES
  cmd_lines_sep <- sapply(preamble_lines, function(x) {
    locs <- str_locate_all(x, pattern = "\\\\newcommand")[[1]]
    str_sub(x, start = locs[, 1], end =  c(locs[-1, 1] - 1, nchar(x)))
  })
  pkg_lines_sep <- sapply(preamble_lines, function(x) {
    locs <- str_locate_all(x, pattern = "\\\\usepackage")[[1]]
    str_sub(x, start = locs[, 1], end =  c(locs[-1, 1] - 1, nchar(x)))
  })

  pream_lines_sep <- c(pkg_lines_sep, cmd_lines_sep)

  pream_lines_sep %>%
    unlist %>%
    str_trim %>%
    unique %>%
    sort() -> plu

  # Manual fixes.
  plu <- plu[!(plu %in% remove_preamble_lines)]

  # Remove Knitr commands which would be double.
  plu <- plu[!grepl("^(\\\\newcommand\\{\\\\hl)", plu)]

  cat("Writing usepackage and newcommand lines.\n")
  filecon <- file(paste0(outloc, "usepkg_newcommand.tex"))
  writeLines(plu, con = filecon)
  close(filecon)
}


manual_fixes <- c("\\newcommand{\\sumin}{\\sum_{i = 1}^n}",
                  "\\newcommand{\\bX}{\\boldsymbol{\\Theta}}",
                  "\\newcommand{\\bx}{\\boldsymbol{\\theta}}",
                  "\\newcommand{\\thedata}{\\bt, \\bX, \\bd}",
                  "\\newcommand{\\wavg}{\\frac{1}{n} \\sum_{i=1}^n}",
                  "\\usepackage{upquote}}{}")


resfol <- "C:/Dropbox/Research/"

filepaths <- c(circ_glm = paste0(resfol, "BayesMultCircCovariates/Article JMP/EstAndHypTestBayesCircGLM.tex"),
               hypotest = paste0(resfol, "BayesIsop/Spread/JSPI/BayesIsop.tex"),
               flexcmix = paste0(resfol, "IVDRSaccades/Spread/Article/JMP/FlexibleModelingOfSaccadeDirectionDistributions.tex"),
               revrjump = paste0(resfol, "VonMisesMixtureReversibleJump/JSCS/VonMisesMix_RevJump_submission.tex"),
               dpm_crim = paste0(resfol, "AoristicAnalysis/Spread/Article/DealingWithPartiallyObservedCrimeTimes.tex"),
               circbays = paste0(resfol, "circbayes_paper/circbayes_RPackageForBayesianCircularStatistics/circbayes_RPackageForBayesianCircularStatistics.tex"))

read_and_save_multiple_chapters(filepaths, remove_preamble_lines = manual_fixes)
