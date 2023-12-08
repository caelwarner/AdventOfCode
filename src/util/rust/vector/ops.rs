use std::ops;
use bitvec::macros::internal::funty::Fundamental;
use crate::vector::{Vec2, Vec3};

macro_rules! inner_vec2_ops {
    ( $vec:ty, $rhs:ty, $trait_name:ident, $fn_name:ident, $assign_trait_name:ident, $assign_fn_name:ident, $op:tt, $assign_op:tt, $x:tt, $y:tt ) => {
        impl ops::$trait_name<$rhs> for $vec {
            type Output = $vec;

            fn $fn_name(self, rhs: $rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32())
            }
        }

        impl ops::$trait_name<&$rhs> for $vec {
            type Output = $vec;

            fn $fn_name(self, rhs: &$rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32())
            }
        }

        impl ops::$trait_name<$rhs> for &$vec {
            type Output = $vec;

            fn $fn_name(self, rhs: $rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32())
            }
        }

        impl ops::$trait_name<&$rhs> for &$vec {
            type Output = $vec;

            fn $fn_name(self, rhs: &$rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32())
            }
        }

        impl ops::$assign_trait_name<$rhs> for $vec {
            fn $assign_fn_name(&mut self, rhs: $rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
            }
        }

        impl ops::$assign_trait_name<&$rhs> for $vec {
            fn $assign_fn_name(&mut self, rhs: &$rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
            }
        }

        impl ops::$assign_trait_name<$rhs> for &mut $vec {
            fn $assign_fn_name(&mut self, rhs: $rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
            }
        }

        impl ops::$assign_trait_name<&$rhs> for &mut $vec {
            fn $assign_fn_name(&mut self, rhs: &$rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
            }
        }
    };
}

macro_rules! vec2_ops {
    ( $vec:ty; $(($trait_name:ident, $fn_name:ident, $assign_trait_name:ident, $assign_fn_name:ident, $op:tt, $assign_op:tt)),+ ) => {
        $(
        inner_vec2_ops!($vec, $vec, $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, x, y);
        inner_vec2_ops!($vec, (i32, i32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1);
        inner_vec2_ops!($vec, (u32, u32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1);
        inner_vec2_ops!($vec, (usize, usize), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1);
        inner_vec2_ops!($vec, (&i32, &i32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1);
        inner_vec2_ops!($vec, (&u32, &u32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1);
        inner_vec2_ops!($vec, (&usize, &usize), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1);
        )*
    };
}

macro_rules! inner_vec3_ops {
    ( $vec:ty, $rhs:ty, $trait_name:ident, $fn_name:ident, $assign_trait_name:ident, $assign_fn_name:ident, $op:tt, $assign_op:tt, $x:tt, $y:tt, $z:tt ) => {
        impl ops::$trait_name<$rhs> for $vec {
            type Output = $vec;

            fn $fn_name(self, rhs: $rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32(), self.y $op rhs.$z.as_i32())
            }
        }

        impl ops::$trait_name<&$rhs> for $vec {
            type Output = $vec;

            fn $fn_name(self, rhs: &$rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32(), self.y $op rhs.$z.as_i32())
            }
        }

        impl ops::$trait_name<$rhs> for &$vec {
            type Output = $vec;

            fn $fn_name(self, rhs: $rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32(), self.y $op rhs.$z.as_i32())
            }
        }

        impl ops::$trait_name<&$rhs> for &$vec {
            type Output = $vec;

            fn $fn_name(self, rhs: &$rhs) -> Self::Output {
                <$vec>::new(self.x $op rhs.$x.as_i32(), self.y $op rhs.$y.as_i32(), self.y $op rhs.$z.as_i32())
            }
        }

        impl ops::$assign_trait_name<$rhs> for $vec {
            fn $assign_fn_name(&mut self, rhs: $rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
                self.z $assign_op rhs.$z.as_i32();
            }
        }

        impl ops::$assign_trait_name<&$rhs> for $vec {
            fn $assign_fn_name(&mut self, rhs: &$rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
                self.z $assign_op rhs.$z.as_i32();
            }
        }

        impl ops::$assign_trait_name<$rhs> for &mut $vec {
            fn $assign_fn_name(&mut self, rhs: $rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
                self.z $assign_op rhs.$z.as_i32();
            }
        }

        impl ops::$assign_trait_name<&$rhs> for &mut $vec {
            fn $assign_fn_name(&mut self, rhs: &$rhs) {
                self.x $assign_op rhs.$x.as_i32();
                self.y $assign_op rhs.$y.as_i32();
                self.z $assign_op rhs.$z.as_i32();
            }
        }
    };
}

macro_rules! vec3_ops {
    ( $vec:ty; $(($trait_name:ident, $fn_name:ident, $assign_trait_name:ident, $assign_fn_name:ident, $op:tt, $assign_op:tt)),+ ) => {
        $(
        inner_vec3_ops!($vec, $vec, $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, x, y, z);
        inner_vec3_ops!($vec, (i32, i32, i32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1, 2);
        inner_vec3_ops!($vec, (u32, u32, u32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1, 2);
        inner_vec3_ops!($vec, (usize, usize, usize), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1, 2);
        inner_vec3_ops!($vec, (&i32, &i32, &i32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1, 2);
        inner_vec3_ops!($vec, (&u32, &u32, &u32), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1, 2);
        inner_vec3_ops!($vec, (&usize, &usize, &usize), $trait_name, $fn_name, $assign_trait_name, $assign_fn_name, $op, $assign_op, 0, 1, 2);
        )*
    };
}

vec2_ops!(
    Vec2;
    (Add, add, AddAssign, add_assign, +, +=),
    (Sub, sub, SubAssign, sub_assign, -, -=),
    (Mul, mul, MulAssign, mul_assign, *, *=),
    (Div, div, DivAssign, div_assign, /, /=)
);

vec3_ops!(
    Vec3;
    (Add, add, AddAssign, add_assign, +, +=),
    (Sub, sub, SubAssign, sub_assign, -, -=),
    (Mul, mul, MulAssign, mul_assign, *, *=),
    (Div, div, DivAssign, div_assign, /, /=)
);
