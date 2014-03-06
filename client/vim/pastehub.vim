" copy and paste helper for PasteHub.
" Maintainer: Kiyoka Nishiyama <kiyoka@sumibi.org>
" Requirements: PasteHub client.
" How to Install:
"   copy this file to ~/.vim/plugin/

if exists("loaded_pastehub")
    finish
endif
let loaded_pastehub = 1

let s:pastehub_latest_key = ""
let s:pastehub_last_paste = ""

if !exists("g:pastehub_get_program")
	if executable( "pastehubGet" ) == 1
		let g:pastehub_get_program = "pastehubGet"
	else
		let g:pastehub_get_program = "/opt/pastehub/bin/pastehubGet"
	endif
endif

if !exists("g:pastehub_post_program")
	if executable( "pastehubPost" ) == 1
		let g:pastehub_post_program = "pastehubPost" 
	else
		let g:pastehub_post_program = "/opt/pastehub/bin/pastehubPost"
	endif
endif

func! PastehubExecutable(prog)
	if executable(a:prog) != 1
		if has("gui_running") && has("dialog_gui")
			call confirm("The command \"" . a:prog . "\" could not be found in your $PATH.")
			return ''
		else
			echo "The command \"" . a:prog . "\" could not be found in your $PATH."
			return ''
		endif
	endif
	return 1
endfunc

func! PastehubSync( key )
	if !PastehubExecutable( g:pastehub_get_program )
		return
	endif
	if !PastehubExecutable( g:pastehub_post_program )
		return
	endif

	" --- post ---
	if @" != s:pastehub_last_paste
		let key    = system( g:pastehub_post_program , @" )
		let s:pastehub_last_paste = @"
		let result = system( "killall -SIGUSR1 Emacs emacs" )
		echo "posting to PasteHub..."
	endif

	" --- get  ---
	if s:pastehub_latest_key != a:key
		let result = system( g:pastehub_get_program . " get" )
		let s:pastehub_latest_key = a:key
		if @" != result
			let @" = result
			echo "PasteHub sync!"
		endif
	endif
endfunc

func! PastehubEventOccur()
	if !PastehubExecutable( g:pastehub_get_program )
		return
	endif
	
	let result = system( g:pastehub_get_program . " time" )

	if v:shell_error
		echo "Problem while executing \"" . g:pastehub_get_program . "\""
		return
	endif

	if (result != s:pastehub_latest_key) || (@" != s:pastehub_last_paste)
		call PastehubSync( result )
	endif
	return result
endfunc

au! CursorHold  * nested call PastehubEventOccur()
au! VimEnter    * nested call PastehubEventOccur()

finish
 
