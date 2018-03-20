defmodule TtlWeb.PageController do
  use TtlWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
