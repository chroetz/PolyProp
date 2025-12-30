using TaylorIntegration
using Arrow
using Tables
using Printf
using Random
using Dates
using JSON
using SHA
using Serialization

# --- GLOBAL SETTINGS ---
setprecision(256)

# --- CLI ARGUMENT PARSING ---
args = Dict(
    "truthName"    => "L63_xd",
    "randomSeed"   => 1,
    "testDuration" => 50.0,
    "nReps"        => 1
)

function parse_cli_args!(args_dict, input_args)
    i = 1
    while i <= length(input_args)
        arg = input_args[i]
        if startswith(arg, "--")
            key = String(arg[3:end])
            if haskey(args_dict, key)
                if i + 1 <= length(input_args)
                    val_str = input_args[i+1]
                    default_type = typeof(args_dict[key])
                    if default_type == Int
                        args_dict[key] = parse(Int, val_str)
                    elseif default_type == Float64
                        args_dict[key] = parse(Float64, val_str)
                    elseif default_type == String
                        args_dict[key] = String(val_str)
                    end
                    i += 1
                else
                    println(stderr, "Warning: No value provided for $key")
                end
            end
        end
        i += 1
    end
end

parse_cli_args!(args, ARGS)

const TRUTH_NAME    = args["truthName"]
const RANDOM_SEED   = args["randomSeed"]
const TEST_DURATION = BigFloat(args["testDuration"])
const N_REPS        = args["nReps"]

# --- CONFIGURATION ---
const TRUTH_DIR  = "../../../PaperTooEasyData/truth"
const OUTPUT_DIR = "../../../PaperTooEasyData/solverError"

# --- HELPER FUNCTIONS ---

function get_seed_str(seed::Int)
    return @sprintf("%04d", seed)
end

function find_file(directory::String, pattern::Regex, description::String)
    files = readdir(directory)
    matches = filter(f -> occursin(pattern, f), files)
    
    if isempty(matches)
        error("No $description file found in '$directory' matching pattern: $pattern")
    elseif length(matches) > 1
        println("Warning: Multiple $description files found. Using the first one: $(matches[1])")
    end
    
    return joinpath(directory, matches[1])
end

# Define Lorenz at global scope for TaylorIntegration performance optimizations
@taylorize function lorenz!(du, u, p, t)
    x, y, z = u
    σ, ρ, β = p
    
    du[1] = σ * (y - x)
    du[2] = x * (ρ - z) - y
    du[3] = x * y - β * z
    return nothing
end

# --- MAIN SCRIPT ---

println("--- Initializing Run ---")
println("Seed: $RANDOM_SEED | Duration: $TEST_DURATION | Truth: $TRUTH_NAME")
Random.seed!(RANDOM_SEED)

if !isdir(OUTPUT_DIR)
    println("Creating output directory: $OUTPUT_DIR")
    mkpath(OUTPUT_DIR)
end

truth_pattern = Regex("$(TRUTH_NAME)_([0-9a-f]+)\\.feather")
feather_path = find_file(TRUTH_DIR, truth_pattern, "Truth Feather")

println("Reading Data from: $feather_path")

if !isfile(feather_path)
    error("File path invalid: $feather_path")
end

input_table = Arrow.Table(feather_path)
cols = Tables.columns(input_table)
col_names = Tables.columnnames(cols)

t_col_in = getproperty(cols, col_names[1])
x_col_in = getproperty(cols, col_names[2])
y_col_in = getproperty(cols, col_names[3])
z_col_in = getproperty(cols, col_names[4])

calculated_dt = Float64(t_col_in[2] - t_col_in[1])
println("Deduced Time Step (dt): $calculated_dt")

t_end_of_file = t_col_in[end]
t_max_start = t_end_of_file - Float64(TEST_DURATION)
valid_indices = findall(t -> t <= t_max_start, t_col_in)

if isempty(valid_indices)
    error("Truth file is too short for requested testDuration ($TEST_DURATION). Max valid start time: $t_max_start")
end

println("Found $(length(valid_indices)) valid starting points.")

# --- SIMULATION LOOP ---

results_store = Dict{Symbol, Vector{Float64}}()
start_times_used = Float64[]

# Define parameters once
σ, ρ, β = BigFloat(10), BigFloat(28), BigFloat(8)/BigFloat(3)
params = (σ, ρ, β)

for rep in 1:N_REPS
    
    println("\n--- Repetition $rep / $N_REPS ---")

    start_idx = rand(valid_indices)
    
    u0 = [
        BigFloat(x_col_in[start_idx]),
        BigFloat(y_col_in[start_idx]),
        BigFloat(z_col_in[start_idx])
    ]
    t_start = BigFloat(t_col_in[start_idx]) 
    t_end_sim = t_start + TEST_DURATION
    
    push!(start_times_used, Float64(t_start))

    println("Selected Start Index: $start_idx")
    println("Simulation Start (t): $t_start")

    # Integration using global lorenz! function
    sol = taylorinteg(lorenz!, u0, t_start, t_end_sim, 28, BigFloat("1e-60"), params, maxsteps=100000000, dense=true)

    # --- ERROR CALCULATION ---
    error_out = Float64[]

    curr_idx = start_idx
    n_rows = length(t_col_in)

    while curr_idx <= n_rows
        t_val_in = t_col_in[curr_idx]
        
        if t_val_in > t_end_sim
            break
        end
        
        u_sim_big = sol(BigFloat(t_val_in)) 
        
        x_sim = Float64(u_sim_big[1])
        y_sim = Float64(u_sim_big[2])
        z_sim = Float64(u_sim_big[3])
        
        x_true = Float64(x_col_in[curr_idx])
        y_true = Float64(y_col_in[curr_idx])
        z_true = Float64(z_col_in[curr_idx])
        
        dist = sqrt((x_sim - x_true)^2 + (y_sim - y_true)^2 + (z_sim - z_true)^2)
        
        push!(error_out, dist)
        
        curr_idx += 1
    end

    results_store[Symbol("rep_$rep")] = error_out
end

# --- SAVE OUTPUTS ---

seed_str = get_seed_str(RANDOM_SEED)
label = "solverError_$(TRUTH_NAME)_x"

sorted_keys = sort(collect(keys(results_store)), by=x->parse(Int, string(x)[5:end]))
table_data = NamedTuple{Tuple(sorted_keys)}([results_store[k] for k in sorted_keys])

io = IOBuffer()
serialize(io, table_data)
data_hash = bytes2hex(sha256(take!(io)))[1:32]

base_filename = "$(label)_$(seed_str)_$(data_hash)"

arrow_path = joinpath(OUTPUT_DIR, "$base_filename.feather")
println("\nSaving data to: $arrow_path")
Arrow.write(arrow_path, table_data)

json_path = joinpath(OUTPUT_DIR, "$base_filename.json")
println("Saving metadata to: $json_path")

meta_data = Dict(
    "randomSeed" => RANDOM_SEED,
    "truthName" => TRUTH_NAME,
    "testDuration" => Float64(TEST_DURATION),
    "solverPrecision" => "x",
    "timeStep" => calculated_dt,
    "nReps" => N_REPS,
    "label" => label,
    "hash" => data_hash,
    "startTimes" => start_times_used,
    "dirPath" => OUTPUT_DIR,
    "name" => "$(label)_$(seed_str)",
    "infoFileName" => json_path,
    "infoFilePath" => OUTPUT_DIR,
    "dataFileName" => arrow_path,
    "dataFilePath" => OUTPUT_DIR
)

open(json_path, "w") do f
    JSON.print(f, meta_data, 4)
end

println("Done.")
