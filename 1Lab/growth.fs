let compound x = (x * (1.0 + 0.05) ** 50.0)

(printf "%f" (compound 2.0))
