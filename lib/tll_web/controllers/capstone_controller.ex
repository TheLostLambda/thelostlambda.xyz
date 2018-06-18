defmodule TllWeb.CapstoneController do
    use TllWeb, :controller

    def index(conn, _params) do
        render conn, "index.html"
    end
end

