type Option(a) do
  | None
  | Some(a)
end

type Map(a) do
  | Empty
  | Entry(string, a, Map(a))
end

def lookup(key: string, map: Map(a)) -> Option(a) do
  match map do
    Empty do None end
    Entry(k, v, next) do
      if k == key do
        Some(v)
      else
        lookup(key, next)
      end
    end
  end
end
