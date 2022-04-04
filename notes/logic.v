Inductive AN : Prop -> Type :=
  | AN_False : AN False
  | AN_True : AN True
  | AN_conj : forall p q, AN p -> AN q -> AN (p /\ q)
  | AN_forall : forall {A : Type} (p : A -> Prop), (forall x, AN (p x)) -> AN (forall x, p x)
  | AN_imply : forall p q, AN q -> AN (p -> q).

Theorem AN_stable (p : Prop) : AN p -> ~ ~ p -> p.
Proof.
  intro H.
  induction H ; firstorder.
Qed.

Definition decidable {A : Set} (p : A -> Prop) := forall x, p x \/ ~ p x.

Definition stable {A : Set} (p : A -> Prop) := forall x, ~ ~ p x -> p x.

Lemma decidable_stable {A : Set} (p : A -> Prop) : decidable p -> stable p.
Proof.
  firstorder.
Qed.
