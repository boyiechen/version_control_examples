#' Load .env

dotenv::load_dot_env(file = ".env")
DROPBOX_PATH = Sys.getenv("DROPBOX_PATH")

# Create directories if they don't exist
dir.create("./result")
dir.create("./data")

