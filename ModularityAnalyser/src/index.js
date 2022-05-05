import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

// app.ports.storeCheckpoint.subscribe(function(value){
//   if(value === null){
//     localStorage.setItem("Checkpoint", value)
//   }
// })

//need to get to the files here, that means parsing the folder and creating document tree + File list
// document.getElementById("filepicker").addEventListener("change", function (event) {
//   // let output = document.getElementById("listing");
//   // let files = event.target.files;
//   // let arr = [];

//   // for (let i = 0; i < files.length; i++) {
//   //   let item = document.createElement("li");
//   //   item.innerHTML = files[i].webkitRelativePath;
//   //   arr.push(files[i].webkitRelativePath);
//   //   output.appendChild(item);
//   //   console.log(item);
//   // };

//   // send parsed tree structure as a JSON to Elm
//   // this crashes ofc
//   // app.ports.getProjectDirTree.send(JSON.parse(arr));

//   // send all the files as JSON to Elm I guess?
//   //console.log(event.target.files)
//   //console.log(event.target.files.webkitRelativePath)
// }, false);

// if(app){
//   app.ports.request.subscribe(() => {
//     app.ports.getProjectDir.send(document.getElementById('project'))
//     console.log(document.getElementById('project'))
//   })
// }

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
