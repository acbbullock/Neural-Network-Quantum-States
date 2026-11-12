module nnqs_images
  !-------------------------------------------------------------------------------------------------------------------
  !! The parallel operations the program uses, across the images of a coarray program.
  !!
  !! Built with `NNQS_COARRAY` defined, these use Fortran's coarray features, and the program runs on as many images as
  !! it is started with. Built without it, the program runs as a single image and needs no coarray support, so it
  !! builds with every compiler. Each operation then does what it does on one image: sums over one image are the
  !! values themselves, and there are no other images to wait for.
  !-------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: rk=>real32
  implicit none (type,external)
  private

  public :: my_image, image_count, sum_images, sync_with_first

  interface sum_images
    !! Replaces each value with its sum over all images, on every image, as `co_sum` does.
    module procedure :: sum_images_real, sum_images_real_1, sum_images_complex_2
  end interface

  contains !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  impure integer function my_image()
    !! This image's index, from 1.
#ifdef NNQS_COARRAY
    my_image = this_image()
#else
    my_image = 1
#endif
  end function my_image

  impure integer function image_count()
    !! The number of images.
#ifdef NNQS_COARRAY
    image_count = num_images()
#else
    image_count = 1
#endif
  end function image_count

  impure subroutine sync_with_first()
    !! The first image waits for every other image, and every other image waits for the first.
#ifdef NNQS_COARRAY
    if ( this_image() == 1 ) then
      sync images (*)
    else
      sync images (1)
    end if
#endif
  end subroutine sync_with_first

  impure subroutine sum_images_real(x)
    real(rk), intent(inout) :: x
#ifdef NNQS_COARRAY
    call co_sum(x)
#else
    x = x
#endif
  end subroutine sum_images_real

  impure subroutine sum_images_real_1(x)
    real(rk), contiguous, intent(inout) :: x(:)
#ifdef NNQS_COARRAY
    call co_sum(x)
#else
    x = x
#endif
  end subroutine sum_images_real_1

  impure subroutine sum_images_complex_2(x)
    complex(rk), contiguous, intent(inout) :: x(:,:)
#ifdef NNQS_COARRAY
    call co_sum(x)
#else
    x = x
#endif
  end subroutine sum_images_complex_2

end module nnqs_images
