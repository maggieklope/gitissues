#' GetIssues
#'
#' Creates a dataframe with information on issues from a GitHub repository
#'
#' @param github_api_endpoint Character. URL for git repo.  Typically in format of /repos/user/name/issues
#' @param url Character. Base URL used to access the API.  Typically "https://api.github.com" for personal accounts, and "https://enterprise address/api/v3" for enterprise accounts
#' @param repo_owner Character. User or enterprise that owns the repo.
#' @param repo_name Character. The name of the repo
#' @param PAT Character. Personal Authentication Token from Github.
#'
#' @return
#' @export
#'
#' @examples
get_issues <- function(github_api_endpoint, url, repo_owner, repo_name, PAT){

  issues_gh <- gh::gh(github_api_endpoint,
                  .api_url = url,
                  owner = repo_owner,
                  repo = repo_name,
                  .token = PAT,
                  state = "all",
                  .limit = "all")

  # exiting and returning message if repo has no issues
  if(length(issues_gh) == 0) stop("there are no issues on repo")

  # getting number of issues to set row
  n_issues <- 1:length(issues_gh)

  # blank data frame
  df_test <- data.frame()

  # creating data frame
  for (i in n_issues) {

    df_temp <- rbind(df_test, data.frame(
      repo_num = i,
      title = issues_gh[[i]]$title,
      creator = issues_gh[[i]]$user$login,
      assignees = ifelse(
        test = is.null(issues_gh[[i]]$assignee$login), # using ifelse() because rbind will remove NA, so would get error of not having enough rows
        yes = NA,
        no = as.character(
          as.data.frame(issues_gh[[i]]$assignees) %>%
            dplyr::select(contains("login")) %>%
            tidyr::unite(col = "assignees", sep = ", "))
      ),
      date_created = as.Date(issues_gh[[i]]$created_at),
      status = issues_gh[[i]]$state,
      date_closed = ifelse(test = is.null(issues_gh[[i]]$closed_at),
                           yes = NA,
                           no = issues_gh[[i]]$closed_at) %>% as.Date(),
      labels = ifelse(
        test = length(issues_gh[[i]]$labels) == 0,
        yes = NA,
        no = as.character(
          purrr::flatten_dfc(issues_gh[[i]]$labels) %>%
            dplyr::select(contains("name")) %>%
            tidyr::unite(col = "labels", sep = ", "))
      ),
      num_comments = issues_gh[[i]]$comments
    ))

  }
  # this option will return a data frame
  return(df_temp)

  # # this option will reutrn a DT html table
  # table <- DT::datatable(df_temp,filter = 'top')
  # return(table)
}
