def main do
  # `y` can mutate only to number types.
  y = 1
  # `x` is local to each branch of the if statement
  # and can be defined to different types.
  if true do
    x = 123
    y = 2
  else
    x = "123"
    y = 3
  end
end
