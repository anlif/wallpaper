wallpaper: Wallpaper.hs
	ghc Wallpaper.hs -o wallpaper
	rm -rf *.o *.hi
clean:
	rm -rf *.o *.hi wallpaper
