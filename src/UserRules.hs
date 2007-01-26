-- stub module to add your own rules.
module UserRules(userRules) where

import RuleUtils(RuleDef) -- gives some examples

import qualified RuleFunctorM
import qualified RuleMonoid
import qualified RuleUtility
import UserRuleArbitrary
import UserRuleBinary
import UserRuleBitsBinary
import UserRuleGhcBinary
import UserRulesGeneric
import UserRuleXml


-- add your rules to this list
userRules :: [RuleDef]
userRules =
    userRulesXml ++
    userRulesBinary ++
    userRulesBitsBinary ++
    userRulesGeneric ++
    userRulesGhcBinary  ++
    RuleUtility.rules ++
    RuleFunctorM.rules ++
    RuleMonoid.rules ++
    UserRuleArbitrary.rules

