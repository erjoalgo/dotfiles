#!/usr/bin/octave -qf

function white2alpha_fn(input_file, output_file, threshold)
  img = imread(input_file);

  ## convert to grayscale
  ## gray = mean(img, 3) ./ 255;

  ## TODO parameterize threshold
  ## white/back threshold
  thresh = img > threshold;
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
  printf ("usage: firma-transparente.m [original_signature_png] [output_png] [threshold]");
endfunction
  
arg_list = argv ();
nargs = size(arg_list, 1);
if nargs > 0 && strcmp(arg_list{1}, "-h") || strcmp(arg_list{1}, "--help");
  usage();
  exit(0);
else if nargs != 2 &&  nargs != 3;
  usage();
  exit(1);
else
  input = arg_list{1};
  output_png = arg_list{2};
  if nargs >= 3;
    threshold = str2double(arg_list{3});
  else
    threshold = 140;
  end
  white2alpha_fn(input, output_png, threshold);
end


## Local Variables:
## compile-command: "./white2alpha.m /home/ealfonso/Downloads/IMG_20190217_164012460_HDR.jpg /tmp/test.png"
## End:
