# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :ttl,
  namespace: TTL,
  ecto_repos: [TTL.Repo]

# Configures the endpoint
config :ttl, TTLWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "ef7eejkmTf1EoxUrR4oxXOEBGbuEuQU+DAnOewX0wf/0R7xtHjdjmqvc8FRhDN47",
  render_errors: [view: TTLWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: TTL.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
