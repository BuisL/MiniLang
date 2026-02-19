inductive Tree (α : Type) where
  | T : α → List (Tree α) → Tree α

namespace Tree

def fmap {α β : Type} (f : α → β) : Tree α → Tree β
  | T x children => T (f x) (children.map (fmap f))

mutual
  theorem fmap_id {α : Type} : ∀ t : Tree α, fmap id t = t
    | T x children => by
        simp[fmap]
        simp [fmap_id_list children]

  theorem fmap_id_list {α : Type} : ∀ ts : List (Tree α), ts.map (fmap id) = ts
    | [] => rfl
    | hd :: tl => by
        simp [fmap_id hd, fmap_id_list tl]
end

mutual
  theorem fmap_comp {α β γ : Type} (f : β → γ) (g : α → β) :
∀ t : Tree α, fmap (f ∘ g) t = (fmap f ∘ fmap g) t
    | T x children => by
        simp[fmap, fmap_comp_list]

  theorem fmap_comp_list {α β γ : Type} (f : β → γ) (g : α → β) :
      ∀ ts : List (Tree α), ts.map (fmap (f ∘ g)) = ts.map (fmap f ∘ fmap g)
    | [] => rfl
    | hd :: tl => by
        simp[fmap_comp f g hd, fmap_comp_list f g tl]
end

-- T (id x) (children.map (fmap id)) = T x children    -- then apply id
-- T x (children.map (fmap id)) = T x children         -- then apply congr 1
-- (children.map (fmap id)) = children                 -- which is equiv to
-- List.map (fmap id) children = children
