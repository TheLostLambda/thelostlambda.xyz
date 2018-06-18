defmodule TllWeb.Router do
  use TllWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", TllWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/capstone", CapstoneController, :index
    get "/photo", PhotoController, :index
  end

  # Other scopes may use custom stacks.
  # scope "/api", TllWeb do
  #   pipe_through :api
  # end
end
