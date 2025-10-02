program main
    implicit none

    interface

    subroutine write_g96(g96_unit, time, timestep, number_of_atoms, &
        xlo, xhi, ylo, yhi, zlo, zhi, xyz, vel)
        implicit none
    
        integer, intent(in) :: g96_unit
        real(kind=8), intent(in) :: time  ! fs
        integer, intent(in) :: timestep
        integer, intent(in) :: number_of_atoms
        real(kind=8), intent(in) :: xlo  ! Angstrom
        real(kind=8), intent(in) :: xhi  ! Angstrom
        real(kind=8), intent(in) :: ylo  ! Angstrom
        real(kind=8), intent(in) :: yhi  ! Angstrom
        real(kind=8), intent(in) :: zlo  ! Angstrom
        real(kind=8), intent(in) :: zhi  ! Angstrom
        real(kind=8), intent(in) :: xyz(3, number_of_atoms)  ! Angstrom
        real(kind=8), intent(in), optional :: vel(3, number_of_atoms)  ! Angstrom/fs

    end subroutine write_g96

    end interface

    !> anchors
    character(len=*), parameter :: anchor_time = "ITEM: TIME"
    character(len=*), parameter :: anchor_timestep = "ITEM: TIMESTEP"
    character(len=*), parameter :: anchor_number_of_atoms = &
        "ITEM: NUMBER OF ATOMS"
    character(len=*), parameter :: anchor_box_bounds = "ITEM: BOX BOUNDS"
    character(len=*), parameter :: anchor_atoms = "ITEM: ATOMS"

    !> command line arguments
    integer :: argc
    
    !> file
    character(len=80) :: lammpstrj_file
    integer, parameter :: lammpstrj_unit = 100
    integer, parameter :: g96_unit = 200
    integer :: stat

    !> lammpstrj contents
    character(len=80) :: line
    real(kind=8) :: time  ! fs
    integer :: timestep
    integer :: number_of_atoms
    real(kind=8) :: xlo  ! Angstrom
    real(kind=8) :: xhi  ! Angstrom
    real(kind=8) :: ylo  ! Angstrom
    real(kind=8) :: yhi  ! Angstrom
    real(kind=8) :: zlo  ! Angstrom
    real(kind=8) :: zhi  ! Angstrom
    logical :: has_vel
    integer, allocatable :: id(:)
    integer, allocatable :: type(:)
    real(kind=8), allocatable :: xyz(:, :)  ! Angstrom
    real(kind=8), allocatable :: vel(:, :)  ! Angstrom/fs

    !> summary
    integer :: number_of_frames
    integer :: time_start
    integer :: time_end
    integer :: time_total

    !> loop index
    integer :: i

    !> parse command line arguments
    argc = iargc()

    if (argc .ne. 1) then
        print "(A)", "Usage: lammpstrj2g96 md.lammpstrj"
        stop
    end if

    print "(A)", "NOTE: This converter only supports lammpstrj dumped by:"
    print "(A)", ""
    print "(A)", "    # w/ velocity"
    print "(A)", "    dump ID group-ID custom N file id type x y z vx vy vz"
    print "(A)", "    # w/o velocity"
    print "(A)", "    dump ID group-ID custom N file id type x y z"
    print "(A)", "    # sorted by id and include time"
    print "(A)", "    dump_modify ID sort id time yes"
    print "(A)", ""
    print "(A)", "If there already exits a lammpstrj that contains coordinates"
    print "(A)", "w/ or w/o velocity, rerun to dump a compatible one."
    print "(A)", ""
    print "(A)", "    # w/ velocity"
    print "(A)", "    rerun previous.lammpstrj dump x y z vx vy vz"
    print "(A)", "    # w/o velocity"
    print "(A)", "    rerun previous.lammpstrj dump x y z"
    print "(A)", ""

    call getarg(1, lammpstrj_file)

    !> read lammpstrj file
    open(unit=lammpstrj_unit, file=trim(lammpstrj_file), status="old")
    !> open g96 file to write
    open(unit=g96_unit, &
        file=lammpstrj_file(1: index(lammpstrj_file, ".lammpstrj") - 1) &
        // ".g96", status="replace")

    !> read contents
    number_of_frames = 0
    call system_clock(time_start)

    do
        !> read time
        read(unit=lammpstrj_unit, fmt="(A)", iostat=stat) line

        ! check EOF
        if (stat .ne. 0) then
            exit
        end if
        
        if (index(trim(line), anchor_time) .gt. 0) then
            read(unit=lammpstrj_unit, fmt=*, iostat=stat) time
            !> debug
            ! print *, "Time = ", time * 1.0d-03, "ps"
        end if

        !> read timestep
        read(unit=lammpstrj_unit, fmt="(A)", iostat=stat) line

        if (index(trim(line), anchor_timestep) .gt. 0) then
            read(unit=lammpstrj_unit, fmt=*, iostat=stat) timestep
            !> debug
            ! print *, "Timestep = ", timestep
        end if

        !> read number of atoms
        read(unit=lammpstrj_unit, fmt="(A)", iostat=stat) line

        if (index(trim(line), anchor_number_of_atoms) .gt. 0) then
            read(unit=lammpstrj_unit, fmt=*, iostat=stat) number_of_atoms
            !> debug
            ! print *, "Number of atoms = ", number_of_atoms
        end if

        !> read box bounds
        read(unit=lammpstrj_unit, fmt="(A)", iostat=stat) line

        if (index(trim(line), anchor_box_bounds) .gt. 0) then
            read(unit=lammpstrj_unit, fmt=*, iostat=stat) xlo, xhi
            read(unit=lammpstrj_unit, fmt=*, iostat=stat) ylo, yhi
            read(unit=lammpstrj_unit, fmt=*, iostat=stat) zlo, zhi
            !> debug
            ! print *, "xlo = ", xlo
            ! print *, "xhi = ", xhi
            ! print *, "ylo = ", ylo
            ! print *, "yhi = ", yhi
            ! print *, "zlo = ", zlo
            ! print *, "zhi = ", zhi
        end if

        !> read atoms: id, type, x, y, z, [vx, vy, vz] (optional)
        read(unit=lammpstrj_unit, fmt="(A)", iostat=stat) line

        if (index(trim(line), anchor_atoms) .gt. 0) then
            !> check velocity
            if (index(trim(line), "vx vy vz") .gt. 0) then
                has_vel = .true.
                !> debug
                ! print *, "Found velocity"
            end if

            if (.not. allocated(id)) then
                allocate(id(number_of_atoms))
                allocate(type(number_of_atoms))
                allocate(xyz(3, number_of_atoms))

                if (has_vel) then
                    allocate(vel(3, number_of_atoms))
                end if
            end if

            do i = 1, number_of_atoms
                if (has_vel) then
                    read(unit=lammpstrj_unit, fmt=*, iostat=stat) &
                        id(i), type(i), xyz(:, i), vel(:, i)
                else
                    read(unit=lammpstrj_unit, fmt=*, iostat=stat) &
                        id(i), type(i), xyz(:, i)
                endif
            end do
        end if

        !> write g96
        if (has_vel) then
            call write_g96(g96_unit, time, timestep, number_of_atoms, &
            xlo, xhi, ylo, yhi, zlo, zhi, xyz, vel)
        else
            call write_g96(g96_unit, time, timestep, number_of_atoms, &
            xlo, xhi, ylo, yhi, zlo, zhi, xyz)
        end if

        number_of_frames = number_of_frames + 1
    end do

    call system_clock(time_end)
    time_total = time_end - time_start

    close(lammpstrj_unit)
    close(g96_unit)

    !> summary
    print *, "Summary"
    print *, "Number of frames: ", number_of_frames
    print *, "Time: ", time_total * 1.0d-03, "s"
    print *, "Speed: ", number_of_frames / (time_total * 1.0d-03), "frame/s"

end program


subroutine write_g96(g96_unit, time, timestep, number_of_atoms, &
    xlo, xhi, ylo, yhi, zlo, zhi, xyz, vel)
    implicit none

    integer, intent(in) :: g96_unit
    real(kind=8), intent(in) :: time  ! fs
    integer, intent(in) :: timestep
    integer, intent(in) :: number_of_atoms
    real(kind=8), intent(in) :: xlo  ! Angstrom
    real(kind=8), intent(in) :: xhi  ! Angstrom
    real(kind=8), intent(in) :: ylo  ! Angstrom
    real(kind=8), intent(in) :: yhi  ! Angstrom
    real(kind=8), intent(in) :: zlo  ! Angstrom
    real(kind=8), intent(in) :: zhi  ! Angstrom
    real(kind=8), intent(in) :: xyz(3, number_of_atoms)  ! Angstrom
    real(kind=8), intent(in), optional :: vel(3, number_of_atoms)  ! Angstrom/fs

    integer :: i

    !> write title
    write(g96_unit, fmt="(A)") "TITLE"
    write(g96_unit, fmt="(A)") ""
    write(g96_unit, fmt="(A)") "END"

    !> write time
    write(g96_unit, fmt="(A)") "TIMESTEP"
    !> 1.0 fs = 1.0e-03 ps
    write(g96_unit, fmt="(I15,F15.6)") timestep, time * 1.0d-03
    write(g96_unit, fmt="(A)") "END"

    !> write position
    write(g96_unit, fmt="(A)") "POSITIONRED"

    do i = 1, number_of_atoms
        !> 1.0 Angstrom -> 0.1 nm
        write(g96_unit, fmt="(3F15.9)") (xyz(1, i) - xlo) * 0.1d0, &
            (xyz(2, i) - ylo) * 0.1d0, (xyz(3, i) - zlo) * 0.1d0
    end do

    write(g96_unit, fmt="(A)") "END"

    !> write velocity
    if (present(vel)) then
        write(g96_unit, fmt="(A)") "VELOCITYRED"

        do i = 1, number_of_atoms
            !> 1.0 Angstrom/fs -> 100.0 nm/ps
            write(g96_unit, fmt="(3F15.9)") vel(:, i) * 100.0d0
        end do

        write(g96_unit, fmt="(A)") "END"
    end if

    !> write box
    write(g96_unit, fmt="(A)") "BOX"
    !> 1.0 Angstrom -> 0.1 nm
    write(g96_unit, fmt="(3F15.9)") (xhi - xlo) * 0.1d0, (yhi - ylo) * 0.1d0, &
        (zhi - zlo) * 0.1d0
    write(g96_unit, fmt="(A)") "END"

end subroutine write_g96
