# elm-parcel-starter

## Setup
**Install yarn package manager for your system**  
_Windows_  
1. Install Choco (will need local admin)
2. Install yarn: `Choco install yarn`

_Linux_  
1. Check your system's package manager, yarn should be available
2. `yarn <cmd>` (to run stuff)  

**Add `yarn` commands to your path**  
When you install yarn or npm packages globally, usually they get added to a `bin` folder here:  
`/home/:user/(.yarn|.npm)/bin`  
Ensure this is added to your path:  
`export PATH="$PATH:/home/:user/(.yarn|.npm)/bin`

**Install the parcel module bundler**  
```bash
yarn global add parcel-bundler
```  

**Install `elm` command-line tools**  
```bash
yarn global add elm elm-test elm-format
```  

**Add `ElmLS` extension to your editor**  
_Visual Studio Code_  
1. Install `ElmLS`
2. Enable the following settings
   - `elmLS.trace.server`: Enable/disable trace logging of client and server communication
   - `elmLS.elmPath`: The path to your elm executable (should work if installed globally, this is set `elm` by default)
   - `elmLS.elmFormatPath`: The path to your elm-format executable (should work if installed globally, it's just `elm-format`)
   - `elmLS.elmTestPath`: The path to your elm-test executable (should work if installed globally, it's just `elm-test`)

**Run the project**
```bash
yarn start
```  

## Troubleshooting Tips
Globally installed npm modules may not work out-of-the-box with linux or osx.  
Add the yarn `bin` folder to your path  
```bash
export PATH="$(yarn global bin):$PATH"
```
