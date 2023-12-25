import z3

infile = open("input.txt")

pos = []
vel = []

for line in infile:
    line = line.strip()
    p, v = line.split(" @ ")
    pos.append(tuple(map(float, p.split(", "))))
    vel.append(tuple(map(float, v.split(", "))))

x = z3.Real('x')
y = z3.Real('y')
z = z3.Real('z')
vx = z3.Real('vx')
vy = z3.Real('vy')
vz = z3.Real('vz')

s = z3.Solver()

for i in range(len(pos)):
    x_i, y_i, z_i = pos[i]
    vx_i, vy_i, vz_i = vel[i]
    t_i = z3.Real(f"t_{i}")
    s.add(x_i + vx_i * t_i == x + vx * t_i)
    s.add(y_i + vy_i * t_i == y + vy * t_i)
    s.add(z_i + vz_i * t_i == z + vz * t_i)

print(s.check())
m = s.model()
print(m[x], m[y], m[z])
print(m[x].as_long() + m[y].as_long() + m[z].as_long())