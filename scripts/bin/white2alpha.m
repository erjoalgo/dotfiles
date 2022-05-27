#!/usr/bin/octave -qf

function [] = white2alpha(input_file, output_file)
  img = imread(input_file);

  ## convert to grayscale
  ## gray = mean(img, 3) ./ 255;

  ## TODO parameterize threshold
  ## white/back threshold
  THRESH = 255/2;
  thresh = img > THRESH;
  tmp_file = sprintf("/tmp/white-black.png");
  imwrite(thresh, tmp_file);
  cmd = sprintf("convert -transparent white %s %s",
                tmp_file, output_file);
  [status, err] = system(cmd);
  if status~=0;
    printf("error making image transparent: %s", err);
  else
    printf("successfully wrote to %s", output_file);
    result = imread(output_file);
    imshow(result);
  end
endfunction

function [] = usage()
  printf ("usage: firma-transparente.m [original_signature_png] [output_png]");
endfunction
  
arg_list = argv ();
nargs = size(arg_list, 1);
if nargs != 2;
  usage();
  if nargs > 0 && strcmp(arg_list{1}, "-h") || strcmp(arg_list{1}, "--help");
    exit(0);
  else
    exit(1);
  end
else
  input = arg_list{1};
  output_png = arg_list{2};
  white2alpha(input, output_png);
end


## Local Variables:
## compile-command: "./white2alpha.m /home/ealfonso/Downloads/IMG_20190217_164012460_HDR.jpg /tmp/test.png"
## End:
