
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'ALLCOMMENTS BACKSLASHTRICKS DOC EXP SMTHELSE STRINGexpression : STRINGexpression : EXPexpression : BACKSLASHTRICKSexpression : DOCexpression : ALLCOMMENTSexpression : SMTHELSE\n    expression : expression expression\n    '
    
_lr_action_items = {'STRING':([0,1,2,3,4,5,6,7,8,],[2,2,-1,-2,-3,-4,-5,-6,2,]),'EXP':([0,1,2,3,4,5,6,7,8,],[3,3,-1,-2,-3,-4,-5,-6,3,]),'BACKSLASHTRICKS':([0,1,2,3,4,5,6,7,8,],[4,4,-1,-2,-3,-4,-5,-6,4,]),'DOC':([0,1,2,3,4,5,6,7,8,],[5,5,-1,-2,-3,-4,-5,-6,5,]),'ALLCOMMENTS':([0,1,2,3,4,5,6,7,8,],[6,6,-1,-2,-3,-4,-5,-6,6,]),'SMTHELSE':([0,1,2,3,4,5,6,7,8,],[7,7,-1,-2,-3,-4,-5,-6,7,]),'$end':([1,2,3,4,5,6,7,8,],[0,-1,-2,-3,-4,-5,-6,-7,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'expression':([0,1,8,],[1,8,8,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expression","S'",1,None,None,None),
  ('expression -> STRING','expression',1,'p_expression_string','main.py',56),
  ('expression -> EXP','expression',1,'p_expression_exp','main.py',60),
  ('expression -> BACKSLASHTRICKS','expression',1,'p_expression_backslashtricks','main.py',64),
  ('expression -> DOC','expression',1,'p_expression_doc','main.py',68),
  ('expression -> ALLCOMMENTS','expression',1,'p_expression_allcomments','main.py',72),
  ('expression -> SMTHELSE','expression',1,'p_expression_SMTHELSE','main.py',76),
  ('expression -> expression expression','expression',2,'p_expression','main.py',82),
]
