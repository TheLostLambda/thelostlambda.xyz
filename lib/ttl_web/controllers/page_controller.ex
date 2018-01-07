defmodule TTLWeb.PageController do
  use TTLWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
