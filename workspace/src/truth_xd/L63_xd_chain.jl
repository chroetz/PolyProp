using TaylorIntegration
using Arrow
using Tables
using Serialization

# --- 0. CONFIGURATION & ARGS ---
chunk_id = length(ARGS) > 0 ? parse(Int, ARGS[1]) : 1
chunk_size = 10  # System time units per job
total_time = 9012

# Precision setup
setprecision(256) 

# File paths
checkpoint_file = "L63_xd_checkpoint.jls" # Stores the exact BigFloat state
output_arrow = "L63_xd_chunk_$(chunk_id).feather"

# --- 1. LORENZ SYSTEM DEFINITION ---
function lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    du[1] = σ * (y - x)
    du[2] = x * (ρ - z) - y
    du[3] = x * y - β * z
    return nothing
end

# --- 2. INITIALIZATION LOGIC ---
σ, ρ, β = BigFloat(10), BigFloat(28), BigFloat(8)/BigFloat(3)
params = (σ, ρ, β)

# Calculate time span for this specific chunk
t_start = BigFloat((chunk_id - 1) * chunk_size)
t_end   = BigFloat(chunk_id * chunk_size)
dt_out  = BigFloat(2)^-10

println("--- Starting Chunk $chunk_id ---")
println("Time interval: [$t_start, $t_end]")

# Determine Initial Conditions (u0)
if chunk_id == 1
    println("First chunk: Using default initial conditions.")
    u0 = [BigFloat(-8), BigFloat(7), BigFloat(27)]
else
    if !isfile(checkpoint_file)
        error("Checkpoint file not found! Cannot continue simulation.")
    end
    println("Resuming: Loading state from $checkpoint_file")
    # Deserialize restores the exact BigFloat binary representation
    u0 = deserialize(checkpoint_file)
end

# --- 3. INTEGRATION ---
stats = @timed sol = taylorinteg(lorenz!, u0, t_start, t_end, 28, BigFloat("1e-60"), params, maxsteps=100000000, dense=true)

println("Integration took: $(stats.time) seconds")
println("Memory allocated: $(stats.bytes / 1024^2) MiB")

# --- 4. CHECKPOINTING (CRITICAL STEP) ---
final_state = sol(t_end) 

println("Serializing final state for next job...")
serialize(checkpoint_file, final_state)

# --- 5. EVALUATE & ROUND TO 64-BIT FOR STORAGE ---
println("Processing output for Arrow storage...")

# Generate time steps (exclude start to avoid duplicates when stitching, unless it's chunk 1)
# Typically, we want [t_start, t_end). The next chunk does [t_end, t_next).
if chunk_id == 1
    t_save = collect(t_start:dt_out:t_end)
else
    # Start one step after t_start to avoid duplicate rows
    t_save = collect((t_start + dt_out):dt_out:t_end)
end

n = length(t_save)
t_col = Vector{Float64}(undef, n)
x_col = Vector{Float64}(undef, n)
y_col = Vector{Float64}(undef, n)
z_col = Vector{Float64}(undef, n)

for i in 1:n
    t_val = t_save[i]
    u_val = sol(t_val)
    
    t_col[i] = Float64(t_val)
    x_col[i] = Float64(u_val[1])
    y_col[i] = Float64(u_val[2])
    z_col[i] = Float64(u_val[3])
end

table = (time = t_col, x = x_col, y = y_col, z = z_col)
Arrow.write(output_arrow, table)

println("Saved chunk to $output_arrow")