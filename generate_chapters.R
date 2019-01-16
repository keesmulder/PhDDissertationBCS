library(stringr)

out_folder <- "C:/Dropbox/Research/PhDDissertationBCS/chapters/"
glm_path <- "C:/Dropbox/Research/BayesMultCircCovariates/Article JMP/"


inside_curly <- Vectorize(function(string) {
  string <- stringr::str_split(string, "\\{")[[1]][2]
  string <- stringr::str_split(string, "\\}")[[1]][1]
  string
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
                                            outloc = "C:/Dropbox/Research/PhDDissertationBCS/chapters/") {

  # Standard naming.
  if (is.null(names(filepaths))) {
    nms <- paste0("chapter_", seq_along(filepaths))
  } else {
    nms <- names(filepaths)
  }

  # Refactor and save.
  for (filepath_nm in nms) {
    filepath <- filepaths[filepath_nm]
    chapter <- read_and_refactor_chapter(filepath)

    cat("Read chapter '", chapter$title, "'. Processing...\n", sep = "")

    saveRDS(chapter, file = paste0(outloc, filepath_nm, ".rds"))

    filecon <- file(paste0(outloc, filepath_nm, ".tex"))
    writeLines(chapter$text, con = filecon)
    close(filecon)
  }
}

resfol <- "C:/Dropbox/Research/"

chapter_texs <- c(circ_glm = paste0(resfol, "BayesMultCircCovariates/Article JMP/EstAndHypTestBayesCircGLM.tex"),
                  hypotest = paste0(resfol, "BayesIsop/Spread/JSPI/BayesIsop.tex"),
                  flexcmix = paste0(resfol, "IVDRSaccades/Spread/Article/JMP/FlexibleModelingOfSaccadeDirectionDistributions.tex"),
                  revrjump = paste0(resfol, "VonMisesMixtureReversibleJump/JSCS/VonMisesMix_RevJump_submission.tex"),
                  dpm_crim = paste0(resfol, "AoristicAnalysis/Spread/Article/DealingWithPartiallyObservedCrimeTimes.tex"),
                  circbays = paste0(resfol, "circbayes_paper/circbayes_RPackageForBayesianCircularStatistics/circbayes_RPackageForBayesianCircularStatistics.tex"))

read_and_save_multiple_chapters(chapter_texs)
