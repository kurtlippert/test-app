# Elm Parcel Starter
Example front-end form for the IP database  

## Setup
**To install yarn package manager, see the link here:**  
https://yarnpkg.com/lang/en/docs/install/#debian-stable  
(This will get you started with yarn whatever your environment)

**Install the parcel module bundler**  
```bash
yarn global add parcel-bundler
```  
**Note** that globally installed modules won't work out-of-the-box with linux or osx. You'll need to add the yarn `bin` folder to your path  
```bash
export PATH="$(yarn global bin):$PATH"
```

**Run the project with parcel**
```bash
parcel index.html
```