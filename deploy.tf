# Git repo for monorepo-take-2.

terraform {
  backend "s3" {
    bucket = "terraform-backend.erosson.org"
    key    = "elm-decimal"
    region = "us-east-1"
  }
}

provider "gitlab" {
  version = "~> 1.0"
}

resource "gitlab_project" "git" {
  name             = "elm-decimal"
  description      = "decimal.js for elm"
  visibility_level = "public"
  default_branch   = "master"

  provisioner "local-exec" {
    command = <<EOF
sh -euo pipefail
git remote remove origin || true
git remote add origin ${gitlab_project.git.ssh_url_to_repo}
git push -u origin master
git pull origin master
EOF
  }
}
