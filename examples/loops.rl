def count_down(n: number) do
  while n > 1 do
    output(n)
    n = n - 1
  end
end

def count_up(n: number) do
  i = n
  while i < n do
    output(i)
    i = i + 1
  end
end

def main do
  count_down(10)
  count_up(20)
end
