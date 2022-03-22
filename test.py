positions = ["n", "s", "e", "w", "c"]
state = ["empty", "can", "wall"]

permutations = []
for ns in state:
	for ss in state:
		for es in state:
			for ws in state:
				for cs in state:
					permutations.append(f"n-{ns} s-{ss} e-{es} w-{ws} c-{cs}")

print("\n".join(permutations))
