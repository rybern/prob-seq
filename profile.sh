stack build --profile && stack exec -- profiling +RTS -p && profiteur profiling.prof && google-chrome-stable profiling.prof.html
