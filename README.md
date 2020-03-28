# Elm Starter Kit (with Parcel and Bulma!)

## Setup

```bash
yarn

# OR

npm install
```

### Add `ElmLS` extension to your editor

Visual Studio Code

1. Install `ElmLS`
2. Enable the following settings
   - `elmLS.trace.server`: Enable/disable trace logging of client and server communication
   - `elmLS.elmPath`: The path to your elm executable (should work if installed globally, this is set `elm` by default)
   - `elmLS.elmFormatPath`: The path to your elm-format executable (should work if installed globally, it's just `elm-format`)
   - `elmLS.elmTestPath`: The path to your elm-test executable (should work if installed globally, it's just `elm-test`)

### Run the project

```bash
yarn start
```

## Automated Management of the `elm.json` file

```bash
# Update 'elm.json' dependencies
elm-json upgrade

# Install new package
elm-json install <PACKAGE_NAME>

# For more commands
elm-json --help
```

## Troubleshooting Tips

Globally installed npm modules may not work out-of-the-box with linux or osx.  
Add the yarn `bin` folder to your path  

```bash
export PATH="$(yarn global bin):$PATH"
```

## TODOs

Because I'd just like these tasks to stick with the code...

### Mock Server

#### Notes

- JSON Server? Docker Container?
- Would need an Express server (or of course the JSON server for something quick)
  - Lets see what can be done with just the **JSON Server**

#### TODO

- Set up JSON Server
  - JSON objects, some faker data?
