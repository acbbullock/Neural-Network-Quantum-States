using CSV, DataFrames, StatsPlots

function main()
  energies = CSV.read("energies.csv", DataFrame, header=1)
  correlations = CSV.read("correlations.csv", DataFrame, header=true)

  @df energies plot( :Energy, leg=false, title="n = "*string(nrow(correlations)), xlabel="epoch", ylabel="⟨H⟩", 
                     dpi=300 )
  savefig("energies.png")

  if ( mod(nrow(correlations), 2) == 1 )
    @df correlations plot( -div(nrow(correlations), 2):div(nrow(correlations), 2), correlations[:,end], leg=false, 
                            title="n = "*string(nrow(correlations)), xlabel="site", ylabel="correlations", ylims=(-1,1), 
                            dpi=300 )
    savefig("correlations.png")

    anim = @animate for i ∈ 1:ncol(correlations)
      @df correlations plot( -div(nrow(correlations), 2):div(nrow(correlations), 2), correlations[:,i], leg=false, 
                              title="n = "*string(nrow(correlations)), xlabel="site", ylabel="correlations", ylims=(-1,1), 
                              dpi=100 )
    end
    gif(anim, "correlations.gif", fps = 20)
  elseif ( mod(nrow(correlations), 2) == 0 )
    @df correlations plot( -div(nrow(correlations), 2):div(nrow(correlations), 2) - 1, correlations[:,end], leg=false, 
                            title="n = "*string(nrow(correlations)), xlabel="site", ylabel="correlations", ylims=(-1,1), 
                            dpi=300 )
    savefig("correlations.png")

    anim = @animate for i ∈ 1:ncol(correlations)
      @df correlations plot( -div(nrow(correlations), 2):div(nrow(correlations), 2) - 1, correlations[:,i], leg=false, 
                              title="n = "*string(nrow(correlations)), xlabel="site", ylabel="correlations", ylims=(-1,1), 
                              dpi=100 )
    end
    gif(anim, "correlations.gif", fps = 20)
  end
end
