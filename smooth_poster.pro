; code to smooth a posterstamp

pro smooth_poster, image_file

  filein = image_file +".fits"

  im = readfits(filein,header1)
  
  im_sm = filter_image(im, smooth=2, fwhm_gaussian = 0.5, /all)

  fileout = image_file + "_sm2.fits"
  writefits, fileout, im_sm, header1

end
