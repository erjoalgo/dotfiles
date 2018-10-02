#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const minimist = require('minimist');

var ARGV_START = 2;

let args = minimist(process.argv.slice(2), {
    alias: {
      h: 'help',
      t: "text",
      s: "size",
      o: "output",
      f: "identfun",
        // v: 'version'
    },
  default: {
    size: 150,
    identfun: "identicon_gen"
  }
});

var text, size, output_sans_ext, identfun;

var IDENT_FUNS = [["identicon_gen", identicon_gen],
                  ["jdenticon_gen", jdenticon_gen]];

function usage (  ) {
  console.log( "args: " );
  for (var k in args) {
    console.log( k+": "+args[k] );
  }
  console.log( "" );

  console.log( "usage: genident.js [-h] -t text [-s size] [-f output]" );
  // TODO package...
  console.log( "" );
  console.log( "$ npm install -g minimist identicon jdenticon" );
  console.log( "$ apt-get install libcairo2-dev libjpeg8-dev "+
               "libpango1.0-dev libgif-dev build-essential g++" );
}


if (args.help) {
  usage();
  process.exit(0);
} else if (args.text == null) {
  usage();
  throw "must provide text to encode";
} else  {
  var fun_idx = IDENT_FUNS.map(function(name_fun){return name_fun[0];}).indexOf(args.identfun);
  if (fun_idx == -1) {
    usage();
    throw "unknown ident function: "+args.identfun
  } else  {
    identfun = IDENT_FUNS[fun_idx][1];
    text = args.text;
    size = parseInt(args.size);
    output_sans_ext = args.output || text;
  }
}


function jdenticon_gen ( val, size, output_filename_sans_ext ) {
  var jdenticon = require("jdenticon");
  var output_filename = output_filename_sans_ext+".png";
  size = 200;
  var png = jdenticon.toPng(val, size);
  fs.writeFileSync(output_filename, png);
  return output_filename;
}

function identicon_gen ( val, size, output_filename_sans_ext ) {
  var identicon = require('identicon');
  // Asynchronous API

  // npm install -g minimist identicon jdenticon
  // https://www.npmjs.com/package/identicon
  // apt-get install libcairo2-dev libjpeg8-dev libpango1.0-dev libgif-dev build-essential g++

  // fix libpng-12 issue:
  // wget -q -O /tmp/libpng12.deb http://mirrors.kernel.org/ubuntu/pool/main/libp/libpng/libpng12-0_1.2.54-1ubuntu1_amd64.deb   && sudo dpkg -i /tmp/libpng12.deb   && rm /tmp/libpng12.deb

  // Synchronous API
  var output_filename = output_filename_sans_ext+".png";
  var buffer = identicon.generateSync({ id: val, size: size});
  fs.writeFileSync(output_filename, buffer);
  return output_filename;
}


var output_filename = identfun(text, size, output_sans_ext);
console.log( "wrote to "+output_filename);

// Local Variables:
// compile-command: "./genident.js ejalfonso 500"
// End:
