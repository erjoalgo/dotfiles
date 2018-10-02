#!/usr/bin/env node

var fs = require('fs');
var path = require('path');

var ARGV_START = 2;
var args = process.argv.slice(ARGV_START);

var username = args[0];
var size = parseInt(args.length>=2? args[1]: 150);

console.log( "username is: "+username );
console.log( "size is: "+size );

if (username == null) {
  console.log( process.argv );
  throw "username must be defined";
}

var out_dir = "./outs";
function maybe_mkdir ( directory ) {
  if (!fs.existsSync(directory)){
    fs.mkdirSync(directory, 0744);
  }
}
maybe_mkdir(out_dir);

function jdenticon_gen ( val, size, output_directory ) {
  var jdenticon = require("jdenticon");
  var output_filename = path.join(output_directory, val+".png");
  size = 200;
  var png = jdenticon.toPng(val, size);
  fs.writeFileSync(output_filename, png);
  return output_filename;
}

function identicon_gen ( val, size, output_directory ) {
  var identicon = require('identicon');
  // Asynchronous API

  // https://www.npmjs.com/package/identicon
  // apt-get install libcairo2-dev libjpeg8-dev libpango1.0-dev libgif-dev build-essential g++

  // fix libpng-12 issue:
  // wget -q -O /tmp/libpng12.deb http://mirrors.kernel.org/ubuntu/pool/main/libp/libpng/libpng12-0_1.2.54-1ubuntu1_amd64.deb   && sudo dpkg -i /tmp/libpng12.deb   && rm /tmp/libpng12.deb

  // Synchronous API
  var output_filename = path.join(output_directory, val+".png");
  var buffer = identicon.generateSync({ id: val, size: size});
  fs.writeFileSync(output_filename, buffer);
  return output_filename;

  /*
  identicon.generate({ id: val, size: size }, function(err, buffer) {
    if (err) throw err;
    fs.writeFileSync(output_filename, buffer);
    });
  */
}

var funs = [identicon_gen, jdenticon_gen];

for (var i = 0; i<funs.length; i++) {
  var fun = funs[i];
  var fun_name = fun.name;
  try  {
    var dir = path.join(out_dir, fun_name);
    maybe_mkdir(dir);
    var output_filename = fun(username, size, dir);
    console.log( "wrote to "+output_filename);
  }catch (e) {
    console.log( fun_name+" error: ", e );
  }
}

// Local Variables:
// compile-command: "./genident.js ejalfonso 500"
// End:
