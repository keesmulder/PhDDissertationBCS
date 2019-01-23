library(stringr)
library(tidyverse)

out_folder <- "C:/Dropbox/Research/PhDDissertationBCS/chapters/"
glm_path <- "C:/Dropbox/Research/BayesMultCircCovariates/Article JMP/"

# Don't ask me what this regex does.
inside_curly <- Vectorize(function(string) {
  stringr::str_extract(string, "\\{.*?\\}(?!.*\\})")
})

# Read a phd chapter and remove junk.
read_and_refactor_chapter <- function(input_path,
                                      remove_line = NULL,
                                      add_near_line = NULL,
                                      replace_near_line = NULL,
                                      all_text_gsubs = NULL) {
  text <- readLines(input_path)

  # Remove commented out lines.
  strip_text <- text[!grepl("^%", text)]

  for (atg in all_text_gsubs) {
    strip_text <- gsub(atg[1], atg[2], strip_text, ignore.case = TRUE)
  }

  # Information to extract as metadata.
  pkg_lines  <- grep("\\usepackage", strip_text, value = TRUE)
  pkg_lines  <- pkg_lines[!grepl("^\\\\documentclass", pkg_lines)]
  new_cmds   <- grep("newcommand", strip_text, value = TRUE)
  title      <- inside_curly(grep("\\\\title", strip_text, value = TRUE))
  authors    <- inside_curly(grep("\\\\author", strip_text, value = TRUE))
  sections   <- inside_curly(grep("\\\\section", strip_text, value = TRUE))

  # Find abstract.
  if ("\\Abstract{" %in% strip_text) {
    abs_start <- grep("\\\\Abstract\\{", strip_text)
    end_accol <- grep("^\\}", strip_text)
    abs_end   <- end_accol[end_accol > abs_start][1]
    abstract  <- c("\\begin{abstract}",
                   strip_text[(abs_start + 1):(abs_end - 1)],
                   "\\end{abstract}")
  } else {
    abs_start <- grep("\\\\begin\\{abstract\\}", strip_text)
    abs_end   <- grep("\\\\end\\{abstract\\}", strip_text)

    abstract   <- strip_text[abs_start:abs_end]
  }

  # Indices to remove.
  first_sec  <- grep("\\\\section", strip_text, value = FALSE)[1]
  bib_idx    <- grep("\\\\bibliography", strip_text, value = FALSE)
  end_idx    <- grep("\\\\end\\{document\\}", strip_text, value = FALSE)
  header_idx <- 1:(first_sec)

  if ("\\begin{thebibliography}{}" %in% strip_text) {
    thebib_start <- which(strip_text %in% "\\begin{thebibliography}{}")
    thebib_end   <- which(strip_text %in% "\\end{thebibliography}")

    bib_idx <- c(bib_idx, thebib_start:thebib_end)
  }

  body_text <- c(abstract, strip_text[-c(bib_idx, end_idx, header_idx)])


  # Preprocess read text.
  # Remove, add and replace in text.
  for (itr in remove_line) {
    if (any(body_text %in% itr)) {
      body_text <- body_text[-which(body_text %in% itr)]
    }
  }



  for (anl in add_near_line) {
    is_addline <- body_text %in% anl[1]

    if (sum(is_addline) == 1) {

      loc <- which(is_addline) + as.numeric(anl[3])
      cat("    Adding at line ",
          loc, " in ", title,
          ":\n", body_text[loc], "\n with extra line: \n", anl[2])
      body_text <- c(body_text[1:loc],
                     anl[2],
                     body_text[(loc + 1):length(body_text)])
      cat("\n-\n")
    }
  }
  for (rnl in replace_near_line) {
    is_replaceline <- body_text %in% rnl[1]

    if (sum(is_replaceline) == 1) {
      loc <- which(body_text %in% rnl[1]) + as.numeric(rnl[3])
      cat("    Replacing line ",
          loc, " in ", title,
          ":\n", body_text[loc], "\n with: \n", rnl[2])
      body_text <- c(body_text[1:(loc - 1)],
                     rnl[2],
                     body_text[(loc + 1):length(body_text)])
      cat("\n-\n")
    }
  }



  # Split the appendix.
  apx_start <- grepl("\\\\appendix", body_text)
  if (any(apx_start)) {
    apx_idx   <- (which(apx_start)[1]):length(body_text)
    apx_text  <- body_text[apx_idx[-1]]
    body_text <- body_text[-apx_idx]
  } else {
    apx_text <- NA
  }

  # Check if the acknowledgements are now empty.
  if (grepl("FlexibleModelingOfSaccadeDirectionDistributions", input_path)
      | grepl("BayesIsop", input_path)) {
    ack_loc <- grep("section\\{Acknowledgements", body_text)
    body_text <- body_text[-(ack_loc:length(body_text))]
  }


  out <- list(title            = title,
              authors          = authors,
              usepackage_lines = pkg_lines,
              new_commands     = new_cmds,
              appendix         = apx_text,
              sections         = sections,
              text             = body_text)
  class(out) <- c("tex_chapter", "list")
  out
}






# add_near_line is a 3-vector with (the line where to add, the line to add, number to move)
# replace_near_line is a 3-vector with (the line where to add, the line to add, number to move)

read_and_save_multiple_chapters <- function(filepaths,
                                            outloc = "C:/Dropbox/Research/PhDDissertationBCS/",
                                            remove_preamble_lines = character(0),
                                            remove_line = NULL,
                                            add_near_line = NULL,
                                            replace_near_line = NULL,
                                            copy_figs = TRUE,
                                            all_text_gsubs = NULL) {

  outloc_ch <- paste0(outloc, "chapters/")
  outloc_ch

  # Standard naming.
  if (is.null(names(filepaths))) {
    nms <- paste0("chapter_", seq_along(filepaths))
  } else {
    nms <- names(filepaths)
  }

  preamble_lines <- character(0)
  outfig_path <- paste0(outloc, "figure/")

  # Refactor and save.
  for (filepath_nm in nms) {
    filepath <- filepaths[filepath_nm]
    cat("Reading file ", filepath, ":\n", sep = "")
    chapter <- read_and_refactor_chapter(filepath,
                                         remove_line,
                                         add_near_line,
                                         replace_near_line,
                                         all_text_gsubs = all_text_gsubs)
    cat("Read chapter '", chapter$title, "'. Processing...\n", sep = "")


    # Add preamble lines/
    preamble_lines <- c(preamble_lines,
                        chapter$usepackage_lines,
                        chapter$new_commands)

    # Save the full object.
    saveRDS(chapter, file = paste0(outloc_ch, filepath_nm, ".rds"))

    # Save the chapter tex.
    filecon <- file(paste0(outloc_ch, filepath_nm, ".tex"))
    writeLines(chapter$text, con = filecon)
    close(filecon)

    # Save the appendix.
    if (!is.na(chapter$appendix[1])) {
      apx <- chapter$appendix

      # Save the chapter appendixtex.
      filecon <- file(paste0(outloc_ch, filepath_nm, "_appendix.tex"))
      writeLines(apx, con = filecon)
      close(filecon)
    }


    if (copy_figs) {
      cat("Figures...\n")
      last_slash <- stringr::str_locate_all(filepath, "/")[[1]]
      clip_str <- str_sub(filepath, end = last_slash[nrow(last_slash), 1])

      fig_files <- dir(paste0(clip_str, "figure/"), full.names = TRUE)
      file.copy(fig_files, outfig_path)
      fig_files <- dir(paste0(clip_str, "figures/"), full.names = TRUE)
      file.copy(fig_files, outfig_path)

    }

    cat("----------------------\n")
    cat("----------------------\n")
  }


  # COMMAND LINES use Packages
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

  # Output package and newcommand files.
  cat("Writing usepackage and newcommand lines.\n")
  filecon <- file(paste0(outloc, "usepkg_newcommand.tex"))
  writeLines(plu, con = filecon)
  close(filecon)
}


resfol <- "C:/Dropbox/Research/"

filepaths <- c(circ_glm = paste0(resfol, "BayesMultCircCovariates/Article JMP/EstAndHypTestBayesCircGLM.tex"),
               hypotest = paste0(resfol, "BayesIsop/Spread/JSPI/BayesIsop.tex"),
               flexcmix = paste0(resfol, "IVDRSaccades/Spread/Article/JMP/FlexibleModelingOfSaccadeDirectionDistributions.tex"),
               revrjump = paste0(resfol, "VonMisesMixtureReversibleJump/JSCS/VonMisesMix_RevJump_submission.tex"),
               dpm_crim = paste0(resfol, "AoristicAnalysis/Spread/Article/DealingWithPartiallyObservedCrimeTimes.tex"),
               circbays = paste0(resfol, "circbayes_paper/circbayes_RPackageForBayesianCircularStatistics/circbayes_RPackageForBayesianCircularStatistics.tex"))



# These terms will be replaced throughout all the papers.
all_text_gsubs <- list(c("circglmbayes", "circglmbayes"))

remove_preamble_lines <- c("\\newcommand{\\sumin}{\\sum_{i = 1}^n}",
                           "\\newcommand{\\bX}{\\boldsymbol{\\Theta}}",
                           "\\newcommand{\\bx}{\\boldsymbol{\\theta}}",
                           "\\newcommand{\\thedata}{\\bt, \\bX, \\bd}",
                           "\\newcommand{\\wavg}{\\frac{1}{n} \\sum_{i=1}^n}",
                           "\\usepackage{upquote}}{}",
                           "\\usepackage{fullpage}",
                           "\\usepackage{abstract}",
                           "\\usepackage{caption,subcaption}")

add_near_line <- list(
  c("\\end{abstract}", "\\newpage", 0),
  c("\\begin{tabular}{llrrrrrrlrrrrrr}", "\\centerline{", -1),
  c("Model & Fit function & MCMC & Marginal Likelihood & Nested \\\\ \\hline", "\\centerline{", -2),
  c("Model & Fit function & MCMC & Marginal Likelihood & Nested \\\\ \\hline", "}", 11),
  c("\\begin{tabular}{llrrrrrrlrrrrrr}", "}", 22),
  c("\\section{Properties of the Power Batschelet Distribution} \\label{app:powbat}",
    "\\chaptermark{Power Batschelet Properties}", 0),
  c("\\section{Proof of variance overestimation using the aoristic fraction method} \\label{proofvar}",
    "\\chaptermark{Variance overestimation proof}", 0)
)


hypo_bf_line_1 <- " BF_{10} = \\left[ \\int_0^{\\infty} I_0(R_0 \\kp) I_0(\\kp)^{-c} d\\kp \\right]^{-1}  \\int_0^{\\infty} I_0(R_0 \\kp) I_0(\\kp)^{-(n+c)} \\prod_{j = 1}^n \\sum_{i \\neq j }  \\exp \\left\\{ \\kp \\cos(\\theta_j - \\Theta_i) \\right\\} d\\kp."
hypo_bf_line_2 <- " BF_{10} = \\left[ 2 \\pi \\int_0^{\\kp_u} \\sqrt{\\kp A(\\kp) A'(\\kp)} d\\kp \\right]^{-1}  \\int_0^{\\kp_u} \\frac{\\sqrt{\\kp A(\\kp) A'(\\kp) } }{I_0(\\kp)^{n+1} } \\prod_{j = 1}^n \\sum_{i \\neq j }  \\exp \\left\\{ \\kp \\cos(\\theta_j - \\Theta_i) \\right\\} d\\kp."
replace_near_line <- list(
  c("\\label{tableANCOVA}", "\\begin{scriptsize}", -2),
  c("\\label{tableANCOVA}", "\\end{scriptsize}", 14),
  c("The inequality hypothesis tests show a large amount of support for the hypothesis that deaf participants perform better than the controls (\\( BF_{\\mu_{cn} > \\mu_{df}:\\mu_{cn} < \\mu_{df}} = \\) 267.82), and for the hypothesis that deaf participants perform better than sign language interpreters (\\( BF_{\\mu_{in} > \\mu_{df}:\\mu_{in} < \\mu_{df}} = \\) 52.28).",
  "The inequality hypothesis tests show a large amount of support for the hypothesis that deaf participants perform better than the controls (where we have \\( BF_{\\mu_{cn} > \\mu_{df}:\\mu_{cn} < \\mu_{df}} = \\) 267.82), and for the hypothesis that deaf participants perform better than sign language interpreters (\\( BF_{\\mu_{in} > \\mu_{df}:\\mu_{in} < \\mu_{df}} = \\) 52.28).", 0)
  , c("Denoting the normalizing constant of either prior by \\( g = 2 \\pi \\int_0^{\\infty} I_0(R_0 \\kappa)I_0(\\kappa)^{-c} d\\kappa \\), the marginal likelihood for \\( H_1 \\) for these priors is",
      "Denoting the normalizing constant of either prior by \\[ g = 2 \\pi \\int_0^{\\infty} I_0(R_0 \\kappa)I_0(\\kappa)^{-c} d\\kappa, \\] the marginal likelihood for \\( H_1 \\) for these priors is", 0)
  , c(hypo_bf_line_1, "\\begin{align}", -1)
  , c(hypo_bf_line_1, "\\end{align}", 1)
  , c(hypo_bf_line_1,
      " BF_{10} &= \\left[ \\int_0^{\\infty} I_0(R_0 \\kp) I_0(\\kp)^{-c} d\\kp \\right]^{-1} \\times \\\\ &\\int_0^{\\infty} I_0(R_0 \\kp) I_0(\\kp)^{-(n+c)} \\prod_{j = 1}^n \\sum_{i \\neq j }  \\exp \\left\\{ \\kp \\cos(\\theta_j - \\Theta_i) \\right\\} d\\kp.", 0)
  , c(hypo_bf_line_2, "\\begin{align}", -1)
  , c(hypo_bf_line_2, "\\end{align}", 1)
  , c(hypo_bf_line_2,
      " BF_{10} &= \\left[ 2 \\pi \\int_0^{\\kp_u} \\sqrt{\\kp A(\\kp) A'(\\kp)} d\\kp \\right]^{-1} \\times \\\\ & \\int_0^{\\kp_u} \\frac{\\sqrt{\\kp A(\\kp) A'(\\kp) } }{I_0(\\kp)^{n+1} } \\prod_{j = 1}^n \\sum_{i \\neq j }  \\exp \\left\\{ \\kp \\cos(\\theta_j - \\Theta_i) \\right\\} d\\kp.", 0)
  , c("\\{ &85^\\circ, 135^\\circ, 135^\\circ, 140^\\circ, 145^\\circ, 150^\\circ, 150^\\circ, 150^\\circ, 160^\\circ, 285^\\circ, 200^\\circ, 210^\\circ, 220^\\circ, 225^\\circ, 270^\\circ \\},",
      "&\\{ &85^\\circ, 135^\\circ, 135^\\circ, 140^\\circ, 145^\\circ, 150^\\circ, 150^\\circ, 150^\\circ, \\\\ &160^\\circ, 285^\\circ, 200^\\circ, 210^\\circ, 220^\\circ, 225^\\circ, 270^\\circ \\},", 0)
  , c("All code for both the statistical tools, the simulation study and the paper is available online at \\url{https://github.com/keesmulder/BayesMultCircCovariates}.",
      "All code for both the statistical tools, the simulation study and the paper is available online at \\\\ \\url{https://github.com/keesmulder/BayesMultCircCovariates}.", 0)
  , c("The distribution of $\\theta$ is unknown, but let's assume it has a population mean direction $\\mu$. An unbiased estimator of $\\mu$ is given by $\\bar\\theta = \\text{atan2}(\\sum_{i=1}^n \\sin(\\theta_i), \\sum_{i=1}^n \\cos(\\theta_i))$ \\citep{mardia2009directional}. For aoristic data, an unbiased estimator of $\\mu$ is",
      "The distribution of $\\theta$ is unknown, but let's assume it has a population mean direction $\\mu$. An unbiased estimator of $\\mu$ is given by \\[\\bar\\theta = \\text{atan2}(\\sum_{i=1}^n \\sin(\\theta_i), \\sum_{i=1}^n \\cos(\\theta_i))\\] \\citep{mardia2009directional}. For aoristic data, an unbiased estimator of $\\mu$ is", 0)
)

remove_line <- "This work was supported by a ------ grant awarded to ------ from ----- (------)."

read_and_save_multiple_chapters(filepaths,
                                remove_preamble_lines = remove_preamble_lines,
                                add_near_line = add_near_line,
                                replace_near_line = replace_near_line,
                                remove_line = remove_line,
                                all_text_gsubs = all_text_gsubs)


