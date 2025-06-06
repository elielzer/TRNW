!  TRNW.f90 
!
!  FUNCTIONS:
!   WinMain()      - Entry point for the application;
!                    displays the main window; processes the message loop
!   MainWndProc()  - processes messages for the main window
!   CenterWindow() - centers one window over another
!   AboutDlgProc() - processes messages for the about box
!

!****************************************************************************
!
!  FUNCTION: WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!
!  PURPOSE:  Entry point for the application
!
!  COMMENTS: Displays the main window and processes the message loop
!
!****************************************************************************

function WinMain( hInstance, hPrevInstance, lpszCmdLine, nCmdShow )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_WinMain@16' :: WinMain
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'WinMain' :: WinMain
!DEC$ ENDIF

    use user32
    use kernel32
    use TRNWGlobals

    implicit none

    integer(SINT) :: WinMain
    integer(HANDLE) hInstance
    integer(HANDLE) hPrevInstance
    integer(LPWSTR) lpszCmdLine
    integer(SINT)   nCmdShow

    include 'TRNW.fi'

    ! Variables
    type (T_WNDCLASS)       wc
    type (T_MSG)            mesg
    integer*4               ret
    integer(LRESULT)        lret
    integer(BOOL)           bret
    integer(SINT)           iret
    integer(HANDLE)                 haccel

    character(SIZEOFAPPNAME) lpszClassName
    character(SIZEOFAPPNAME) lpszIconName
    character(SIZEOFAPPNAME) lpszAppName
    character(SIZEOFAPPNAME) lpszMenuName
    character(SIZEOFAPPNAME) lpszAccelName

    integer N, I, J, K, M, I2, COMPUT
    integer, dimension(100) :: LISTA
    integer, dimension(200,3) :: LISTS

    integer, dimension(200,2) :: LISTP 
    
    N = 100
    M = 2*N
    
    ghInstance = hInstance
    ghModule = GetModuleHandle(NULL)
    ghwndMain = NULL

    lpszClassName ="TRNW"C
    lpszAppName ="TRNW"C
    lpszIconName ="TRNW"C  
    lpszMenuName ="TRNW"C
    lpszAccelName ="TRNW"C

    !  If this is the first instance of the application, register the
    !  window class(es)
    if (hPrevInstance .eq. 0) then
        !  Main window
         wc%lpszClassName = LOC(lpszClassName)
         wc%lpfnWndProc = LOC(MainWndProc)
         wc%style = IOR(CS_VREDRAW , CS_HREDRAW)
         wc%hInstance = hInstance
         wc%hIcon = LoadIcon( hInstance, LOC(lpszIconName))
         wc%hCursor = LoadCursor( NULL, IDC_ARROW )
         wc%hbrBackground = ( COLOR_WINDOW+1 )
         wc%lpszMenuName = NULL
         wc%cbClsExtra = 0
         wc%cbWndExtra = 0
         if (RegisterClass(wc) == 0) goto 99999
    end if

    ! Load the window's menu and accelerators and create the window
    !
    ghMenu = LoadMenu(hInstance, LOC(lpszMenuName))
    if (ghMenu == 0) goto 99999
    haccel = LoadAccelerators(hInstance, LOC(lpszAccelName))
    if (haccel == 0) goto 99999

    ghwndMain = CreateWindowEx(  0, lpszClassName,                 &
                                 lpszAppName,                      &
                                 INT(WS_OVERLAPPEDWINDOW),         &
                                 CW_USEDEFAULT,                    &
                                 0,                                &
                                 CW_USEDEFAULT,                    &
                                 0,                                &
                                 NULL,                             &
                                 ghMenu,                           &
                                 hInstance,                        &
                                 NULL                              &
                              )
    if (ghwndMain == 0) goto 99999
    
    !*******************************************
    ! CRIA UMA LISTA COM NUMERO QUE FORMAM UM     
    ! TRIÂNGULO RETÂNGULO INTEIRO
    !*******************************************
    
    FORALL (I = 1:M)
        LISTP(I,1) = I
        LISTP(I,2) = I**2
    end forall
    
    FORALL (I = 1:N)
        LISTA(I) = I
    end forall
    
    I = 1
    I2 = 0
    do while (I .le. N)
        J = 1
        do while (J .le. N)
            if (LISTA(I) .NE. LISTA(J)) then
            COMPUT = LISTA(I)**2 + LISTA(J)**2
            K = 1
            do while (K .le. M)
                if (COMPUT.EQ. LISTP(K,2)) then
                I2 = I2 +1
                LISTS(I2,1) = LISTA(I)               
                LISTS(I2,2) = LISTA(J)
                LISTS(I2,3) = LISTP(K,1)
               
                goto 10
                end if
                K = K + 1
            end DO
            end if

            !
10        J = J + 1
        end do
    
        I = I + 1
    end do 
    
    open(1,FILE='test.dat')
    write (1, '(I3,I3,I5)') (LISTS(n,1), LISTS(n,2), LISTS(n,3), n = 1, 100)

    

    

    lret = ShowWindow( ghwndMain, nCmdShow )

    ! Read and process messsages
    do while( GetMessage (mesg, NULL, 0, 0) ) 
       if ( TranslateAccelerator (mesg%hwnd, haccel, mesg) == 0) then
           bret = TranslateMessage( mesg )
           lret  = DispatchMessage( mesg )
       end if
    end do

    WinMain = mesg.wParam
    return

99999 &

    iret = MessageBox(ghwndMain, "Error initializing application TRNW"C, &
                     "Error"C, MB_OK)
    WinMain = 0

end 

!****************************************************************************
!
!  FUNCTION: MainWndProc ( hWnd, mesg, wParam, lParam )
!
!  PURPOSE:  Processes messages for the main window
!
!  COMMENTS:
!
!****************************************************************************

integer(LRESULT) function MainWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_MainWndProc@16' :: MainWndProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'MainWndProc' :: MainWndProc
!DEC$ ENDIF

    use user32
    use TRNWGlobals

    implicit none

    integer(HANDLE) hWnd
    integer(UINT) mesg
    integer(UINT_PTR) wParam
    integer(LONG_PTR) lParam 

    include 'resource.fd'

    interface 
    function  AboutDlgProc( hwnd, mesg, wParam, lParam )
    !DEC$ IF DEFINED(_X86_)
    !DEC$ ATTRIBUTES STDCALL, ALIAS : '_AboutDlgProc@16' :: AboutDlgProc
    !DEC$ ELSE
    !DEC$ ATTRIBUTES STDCALL, ALIAS : 'AboutDlgProc' :: AboutDlgProc
    !DEC$ ENDIF
    use ifwinty
    integer(LRESULT) :: AboutDlgProc ! LRESULT
    integer(HANDLE)   hwnd 
    integer(UINT)     mesg  
    integer(UINT_PTR) wParam
    integer(LONG_PTR) lParam 
    end function 
    end interface

    ! Variables
    integer(BOOL)       lret
    integer(LRESULT)     ret
    character(SIZEOFAPPNAME)  lpszName, lpszHelpFileName, lpszContents, lpszMessage
    character(SIZEOFAPPNAME)  lpszHeader

    select case ( mesg )

    ! WM_DESTROY: PostQuitMessage() is called 
      case (WM_DESTROY)
          call PostQuitMessage( 0 )
          MainWndProc = 0
          return

    ! WM_COMMAND: user command
      case (WM_COMMAND)
        select case ( IAND(wParam, 16#ffff ) )
 
            case (IDM_EXIT)
                ret = SendMessage( hWnd, WM_CLOSE, 0, 0 )
                MainWndProc = 0
                return
  
            case (IDM_ABOUT)
                lpszName = "AboutDlg"C
                ret = DialogBoxParam(ghInstance,LOC(lpszName),hWnd,& 
                  LOC(AboutDlgProc), 0_LONG_PTR)
                MainWndProc = 0
                return

            case (IDM_HELPCONTENTS)  
                lpszHelpFileName ="\\TRNW.hlp"C
                lpszContents = "CONTENTS"C
                if (WinHelp (hWnd, lpszHelpFileName, HELP_KEY, &
                               LOC(lpszContents)) .EQV. FALSE) then
                lpszMessage = "Unable to activate help"C
                lpszHeader = "TRNW"
                ret = MessageBox (hWnd,                             &
                                 lpszMessage,                       &
                                 lpszHeader,                        &
                                 IOR(MB_SYSTEMMODAL,                &
                                 IOR(MB_OK, MB_ICONHAND)))
                end if
                MainWndProc = 0
                return

            case (IDM_HELPSEARCH)
                lpszHelpFileName ="\\TRNW.hlp"C
                lpszContents = "CONTENTS"C
                if (WinHelp(hWnd, "TRNW.hlp"C,            &
                       HELP_PARTIALKEY, LOC(""C)) .EQV. FALSE) then
                   lpszMessage = "Unable to activate help"C
                   lpszHeader = "TRNW"C
                   ret = MessageBox (hWnd,                          &
                                 lpszMessage,                       &
                                 lpszHeader,                        &
                                 IOR(MB_SYSTEMMODAL ,               &
                                 IOR(MB_OK, MB_ICONHAND)))
                end if
                MainWndProc = 0
                return

            case (IDM_HELPHELP)
                if (WinHelp(hWnd, ""C, HELP_HELPONHELP, 0).EQV. FALSE)& 
                                                       then
                   lpszMessage = "Unable to activate help"C
                   lpszHeader = "TRNW"C
                   ret = MessageBox (GetFocus(),                    &
                                 lpszMessage,                       &
                                 lpszHeader,                        &
                                 IOR(MB_SYSTEMMODAL,IOR(MB_OK, MB_ICONHAND)))
                end if
                MainWndProc = 0
                return

            ! All of the other possible menu options are currently disabled

            case DEFAULT
                MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )
                return
        end select

    ! Let the default window proc handle all other messages
      case default
          MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )

    end select

end

!****************************************************************************
!
!  FUNCTION: CenterWindow (HWND, HWND)
!
!  PURPOSE:  Center one window over another
!
!  COMMENTS: Dialog boxes take on the screen position that they were designed
!            at, which is not always appropriate. Centering the dialog over a
!            particular window usually results in a better position.
!
!****************************************************************************

subroutine CenterWindow (hwndChild, hwndParent)

    use user32
    use gdi32 
    use TRNWGlobals

    implicit none

    integer(HANDLE)         hwndChild, hwndParent

    include 'resource.fd'

    ! Variables
    type (T_RECT)   rChild, rParent
    integer         wChild, hChild, wParent, hParent
    integer         wScreen, hScreen, xNew, yNew
    integer(HANDLE)         hdc
    integer*4       retval

    ! Get the Height and Width of the child window
       retval = GetWindowRect (hwndChild, rChild)
       wChild = rChild.right - rChild.left
       hChild = rChild.bottom - rChild.top

    ! Get the Height and Width of the parent window
       retval = GetWindowRect (hwndParent, rParent)
       wParent = rParent.right - rParent.left
       hParent = rParent.bottom - rParent.top

    ! Get the display limits
       hdc = GetDC (hwndChild)
       wScreen = GetDeviceCaps (hdc, HORZRES)
       hScreen = GetDeviceCaps (hdc, VERTRES)
       retval = ReleaseDC (hwndChild, hdc)

    ! Calculate new X position, then adjust for screen
       xNew = rParent.left + ((wParent - wChild) /2)
       if (xNew .LT. 0) then
          xNew = 0
       else if ((xNew+wChild) .GT. wScreen) then
          xNew = wScreen - wChild
       end if

    ! Calculate new Y position, then adjust for screen
       yNew = rParent.top  + ((hParent - hChild) /2)
       if (yNew .LT. 0) then
          yNew = 0
       else if ((yNew+hChild) .GT. hScreen) then
          yNew = hScreen - hChild
       end if

    ! Set it, and return
       retval = SetWindowPos (hwndChild, NULL, xNew, yNew, 0, 0,      &
                      IOR(SWP_NOSIZE , SWP_NOZORDER))
end  

 
!/****************************************************************************
!
!  FUNCTION: AboutDlgProc(HWND, UINT, WPARAM, LPARAM)
!
!  PURPOSE:  Processes messages for "About" dialog box
!
!  COMMENTS: Display version information from the version section of the
!            application resource.  Wait for user to click on "Ok" button,
!            then close the dialog box.
!
!****************************************************************************/

function AboutDlgProc( hDlg, message, uParam, lParam )
!DEC$ IF DEFINED(_X86_)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_AboutDlgProc@16' :: AboutDlgProc
!DEC$ ELSE
!DEC$ ATTRIBUTES STDCALL, ALIAS : 'AboutDlgProc' :: AboutDlgProc
!DEC$ ENDIF

    use kernel32
    use user32
    use gdi32
    use ifwbase
    use version
    use TRNWGlobals

    implicit none

    integer(LRESULT) :: AboutDlgProc ! LRESULT
    integer(HANDLE)     hDlg        ! window handle of the dialog box
    integer(UINT)   message
    integer(UINT_PTR)   uParam
    integer(LONG_PTR)   lParam

    include 'resource.fd'

    ! Variables
    integer(HANDLE)   hfontDlg
    save        hfontDlg

    integer     dwVerHnd
    integer(DWORD)     dwVerInfoSize
    integer(SIZE_T)    sVerInfoSize
    integer     uVersionLen
    integer     bRetCode
    integer     i
    character*1024   szFullPath
    character*1024    szResult
    character*1024   szGetName
    integer(LPVOID)  lpVersion

    integer(LPVOID)   lpstrVffInfo
    integer(HANDLE)   hMem
    integer*4   iData
    integer*4   ret

    select case (message)
      case (WM_INITDIALOG)   ! message: initialize dialog box
         ! Create a font to use
         hfontDlg = CreateFont(14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,& 
                        IOR(INT(VARIABLE_PITCH) , INT(FF_SWISS)), ""C)

         ! Center the dialog over the application window
         call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

         ! Get version information from the application
         ret = GetModuleFileName (ghInstance, szFullPath,     &
                               len(szFullPath))
         dwVerInfoSize = GetFileVersionInfoSize(szFullPath,   &
                                       LOC(dwVerHnd))

         if (dwVerInfoSize .NE. 0) then
            ! If we were able to get the information, process it:
            sVerInfoSize = dwVerInfoSize
            hMem = GlobalAlloc(GMEM_MOVEABLE, sVerInfoSize)
            lpstrVffInfo  = GlobalLock(hMem)
            ret = GetFileVersionInfo (szFullPath, dwVerHnd, &
            dwVerInfoSize, lpstrVffInfo)

            ! Walk through the dialog items that we want to replace:
            do i = IDC_VER1, IDC_VER5
               ret = GetDlgItemText(hDlg, i, szResult,      &     
                             len(szResult))
                
               szGetName = "\\StringFileInfo\\040904E4\\"C               
               ret =lstrcat(szGetName,szResult)

               bRetCode = VerQueryValue(lpstrVffInfo,    &
                                            szGetName,  &
                                            lpVersion,  &
                                            LOC(uVersionLen))

               if ( bRetCode .NE. 0 ) then
                  ! Replace dialog item text with version info
                  ret = lstrcpyn(LOC(szResult),lpVersion, uVersionLen)
                  ret = SetDlgItemText(hDlg, i,szResult)
                  ret = SendMessage (GetDlgItem (hDlg, i),   &
                                   WM_SETFONT, hfontDlg, TRUE)
               end if
            end do 

            ret = GlobalUnlock(hMem)
            ret = GlobalFree(hMem)
         end if 
         AboutDlgProc = 1
         return
      case (WM_COMMAND)                      ! message: received a command
         iData = uParam
         if ((IAND(iData,16#ffff) .EQ. IDOK) & !OK Selected?
            .OR. (IAND(iData,16#ffff) .EQ. IDCANCEL)) then ! Close command?
            ret = EndDialog(hDlg, TRUE)      ! Exit the dialog
            ret = DeleteObject (hfontDlg)
            AboutDlgProc = 1
            return
         end if
    end select  
    AboutDlgProc = 0 ! Didn't process the message
    return
end 

