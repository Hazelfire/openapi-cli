# openapi-cli

This project takes [OpenAPI](https://swagger.io/specification/) specifications
and turns them into command line interfaces. This helps with quickly prototyping
calls to an API. I built this for the purpose of programmatically doing my
Systematic Literature Review for my thesis.

Some example specification OpenAPI specification files (that I used for my review,
so mainly academic database APIs) can be found [here](https://github.com/Hazelfire/openapi-specs).

# Configuration
To configure, you'll need to specify a config file. This config file lives at `~/.config/api/config.yaml`. An example is below:

```yaml
semantic:
  file: specs/semantic.yaml
  defaults: {}
up:
  file: specs/up.yaml
  defaults:
    Authorization:
      command: 'echo -n "Bearer $(pass Up_token)"'
eslevier:
  file: specs/eslevier.yaml
  defaults:
    X-ELS-APIKey:
      command: pass "Eslevier API Key"
```

The file starts with a collection of APIs (In this case, "semantic", "up",
and "eslevier"). Then, the specification file is the location of an OpenAPI spec, and the defaults are what default parameters you want to pass into the commands (Often automatic authorization).

The specification files are placed relative to `~/.config/api/`.

# Running commands

The basic structure of a command is
```
openapi [api] [action] [parameters...]
```

The api is the name of the api (such as "semantic" in the above config). The action is the [operationId](https://swagger.io/docs/specification/paths-and-operations/)
of a path. Then finally parameters is a collection of `--[name] value`
pairs for the parameters of the command.

The utility will return the result, if possible, in YAML for easy viewing
