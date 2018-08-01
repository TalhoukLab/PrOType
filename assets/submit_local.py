import os
import subprocess
import argparse
from multiprocessing.dummy import Pool


def run_process(script):
    print(script)
    subprocess.call([script], shell=True)


def run_all_processes(num_parallel, scripts):
    from random import shuffle
    #shuffle(scripts)

    pool = Pool(num_parallel)
    pool.map(run_process, scripts)


def get_files(root_path):
    found_files = []

    for root, dirs, files in os.walk(root_path):
        path = root.split(os.sep)
        print((len(path) - 1) * '---', os.path.basename(root))
        # Iterate over each file.
        for file in files:
            if file.endswith(".sh"):
                found_files.append("bash " + os.path.join(root, file))

    print("Found: {0} files".format(len(found_files)))
    return found_files


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    configs = {
        "runtime": {
            "num_parallel": {
                "help": "How many to run in parallel",
                "default": 1,
                "type": int
            },
            "file_location": {
                "help": "File path to all shell scripts to run (.../ov.afc2_cbt)",
                "type": str,
                "required": True
            },
            "step": {
                "choices": ["clust", "consensus", "merge", "prep", "train", "iv_summary",
                            "post_processing",
                            "gene_selection_bootstrap",
                            "geneselection_process_bootstrap",
                            "gene_selection_final_training",
                            "geneselection_predict"],
                "help": "Which stage we're on...",
                "required": True
            }
        }
    }

    for g_name, group in configs.items():
        arg = parser.add_argument_group(g_name)

        for conf in group.keys():
            arg.add_argument("--" + str(conf), **group[conf])

    parsed, unparsed = parser.parse_known_args()

    num_parallel = parsed.num_parallel
    file_path = parsed.file_location
    step = parsed.step

    file_path = os.path.join(file_path, "sh_file", step)

    found_files = get_files(file_path)

    run_all_processes(num_parallel, found_files)
